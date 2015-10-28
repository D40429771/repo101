-- FILE NAME..: szptlda.sql
-- RELEASE....: 1.0
-- OBJECT NAME: SZPTLDA
-- PRODUCT....: DeVry Education Group
-- AUTHOR.....: Zack Hu
-- USAGE......: Used by SZPTLDA job that inserts state restrictions for post licensure programs...
--
--
-- Parameters: Run Mode                                    
--                                                                           
-- COPYRIGHT..: Copyright (C) DeVry Education Group 2015. All rights reserved.
/* AUDIT TRAIL
  
  Ver        Date        Author           Description
  -----  ----------  ---------------	------------------------------------------------------------------------------------------------------------------
  1.0	09/28/2015  	Zack Hu     	Initial version per ER13244
  
***********************************************************************************************************************************************************
*/  
SET SERVEROUTPUT ON SIZE UNLIMITED
SET VERIFY OFF
SET FEEDBACK OFF
SET ECHO OFF
SET PAGESIZE 0
SET TRIMSPOOL ON
SET LINESIZE 400
--- sqlplus $UIPW @$BANNER_LINKS/SZPTLDA.sql ${PROC} ${ONE_UP} ${LIS} ${UID}
 SPOOL &3 APPEND
BEGIN
     IF NOT f$_check_security('SZPTLDA') THEN
         DBMS_OUTPUT.PUT_LINE('Unauthorized to execute SZPTLDA');
     END IF;
END;
/
DECLARE 
   security_status    BOOLEAN;
   c_job_name         CONSTANT	VARCHAR2(7) :='SZPTLDA';
   c_line_length      CONSTANT	PLS_INTEGER	:=110; 	    -- report width
   v_user             VARCHAR2(50):=UPPER('&4');
   v_one_up_number    general.gjbprun.gjbprun_one_up_no%TYPE := &2;   
   c_rundate          CONSTANT DATE := SYSDATE;
   v_runmode          CHAR(1);                          -- parameter 1
   
   v_recordins        NUMBER;
   v_recorddel        NUMBER;
   v_errorcnt         NUMBER;
   v_runmode_desc     general.gjbpdef.gjbpdef_desc%TYPE;

   v_header_line_1    VARCHAR2(1000);
   v_header_line_2    VARCHAR2(1000);
   v_header_error     BOOLEAN;
   v_start_time       VARCHAR2(25);
   error_indx         PLS_INTEGER :=0;
   e_noemailaddr      EXCEPTION;
   e_nouserid         EXCEPTION;
   e_norunmode        EXCEPTION;
   e_norunterm        EXCEPTION;
   e_norundate        EXCEPTION;	
   e_header_error     EXCEPTION;	
 
--- more var
-----------------------------------------------------------------------------
-- Procedure to write the sub-header information
-----------------------------------------------------------------------------
	PROCEDURE p_print_sub_header 
	IS
	BEGIN
      DBMS_OUTPUT.NEW_LINE;
      DBMS_OUTPUT.NEW_LINE;
      DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-')); 
      DBMS_OUTPUT.PUT_LINE('Transaction  '||
	  RPAD('Student Name',32,' ')||'Student D#  '||
                     'Term Code  '||'Level Code   '||
					 RPAD('Transcript Comment',32,' '));
      DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-'));	
	END p_print_sub_header;   
---------------------------------------------------
-- Procedure to write the footer information
---------------------------------------------------
	PROCEDURE p_print_footer
	IS
	BEGIN
       DBMS_OUTPUT.PUT_LINE (RPAD('-',c_line_length,'-'));
       DBMS_OUTPUT.NEW_LINE;
       DBMS_OUTPUT.PUT_LINE ('CONTROL REPORT FOR '||c_job_name||' RUN');
       DBMS_OUTPUT.PUT_LINE ('JOB RUN: '||v_one_up_number);
       DBMS_OUTPUT.PUT_LINE ('Processed: '||TO_CHAR(v_recordins + v_recorddel));		
       DBMS_OUTPUT.PUT_LINE ('Errors: '||TO_CHAR(v_errorcnt));
       DBMS_OUTPUT.PUT_LINE (c_job_name||' Started at: '||TO_CHAR(c_rundate,'DD-Mon-YYYY HH12:MI:SS AM'));
       DBMS_OUTPUT.PUT_LINE (c_job_name||' Ended at: '||TO_CHAR(SYSDATE,'DD-Mon-YYYY HH12:MI:SS AM'));
       DBMS_OUTPUT.NEW_LINE;
	
	END p_print_footer;
  
    PROCEDURE szptlda_exec(
      p_user_id     VARCHAR2 DEFAULT NULL,
      p_debug       BOOLEAN DEFAULT FALSE,
      p_record_ins  OUT NUMBER,
	  p_record_del  OUT NUMBER,
      p_error_cnt   OUT NUMBER) AS  
	  
      c_rundate     CONSTANT DATE := SYSDATE;
      c_line_length CONSTANT PLS_INTEGER := 110; 
      c_job         CONSTANT VARCHAR2(20) := 'SZPTLDA';	
      c_legacy_term CONSTANT VARCHAR2(20) := '201240';	
      c_wthdrl      CONSTANT VARCHAR2(20) := 'Withdrawal Date ';
      v_rec         VARCHAR2(2000);
      v_name        VARCHAR2(120);
      v_timing      NUMBER;
      v_idx         NUMBER := 0;
	  v_ddx         NUMBER := 0;
      v_err_idx     NUMBER := 0;
      
      v_pidm        saturn.shrttcm.shrttcm_pidm%TYPE;
      v_user_id     saturn.shrttcm.shrttcm_user_id%TYPE;
      v_dsi         saturn.spriden.spriden_id%TYPE;
      v_term_code   saturn.shrttcm.shrttcm_term_code%TYPE;
      v_levl_code   saturn.shrttcm.shrttcm_levl_code%TYPE;
      v_cmnt        saturn.shrttcm.shrttcm_comment%TYPE;

	  TYPE rec_type IS RECORD (
           pidm         shrttcm.shrttcm_pidm%TYPE
          ,dsi          spriden.spriden_id%TYPE
          ,student_name VARCHAR2(120)
          ,levl_code    shrttcm.shrttcm_levl_code%TYPE
          ,term_code    shrttcm.shrttcm_term_code%TYPE
          ,cmnt         shrttcm.shrttcm_comment%TYPE        
          ,msg          VARCHAR2(200)
          ,err          VARCHAR2(1000));
     TYPE table_rec_type IS TABLE OF rec_type INDEX BY PLS_INTEGER;
     err_rec  table_rec_type;
     log_rec  table_rec_type;
 
----- sfrwdrl_eff_wdrl_date DD-MON-YY
----- shrttcm_comment       DD-Mon-YYYY
--       READ: pk_sfrwdrl = (pidm, term_code, seq_no)
-- READ WRITE: shrttcm has no Unique Index or PK. 
--   NOT NULL: pidm, levl_code, term_code, activity_date. 
--             To match against (pidm, levl_code, term_code)

      CURSOR c_shrttcm_ins IS
        WITH v_result_set AS (
      SELECT w.sfrwdrl_pidm AS pidm,
             w.sfrwdrl_term_code AS term_code,
             TO_CHAR(w.sfrwdrl_eff_wdrl_date, 'DD-Mon-YYYY') AS cmnt
        FROM saturn.sfrwdrl w
       WHERE w.sfrwdrl_term_code >= c_legacy_term
         AND w.sfrwdrl_seq_no = (SELECT MAX(ww.sfrwdrl_seq_no)
                                   FROM saturn.sfrwdrl ww
                                  WHERE ww.sfrwdrl_pidm = w.sfrwdrl_pidm
                                    AND ww.sfrwdrl_term_code = w.sfrwdrl_term_code)										
       MINUS
      SELECT shrttcm_pidm AS pidm,
             shrttcm_term_code AS term_code,
             RTRIM(LTRIM(REPLACE(shrttcm_comment, c_wthdrl))) AS cmnt
        FROM saturn.shrttcm
       WHERE shrttcm_term_code >= c_legacy_term
      )
      SELECT v.pidm AS pidm,
             spriden_id AS dsi,
             INITCAP(SUBSTR(LTRIM(spriden_last_name)||', '||NVL(spriden_first_name,' '),1,30)) AS student_name,
             a.sgbstdn_levl_code AS shrttcm_levl_code,
             v.term_code AS shrttcm_term_code,
             c_wthdrl||TO_CHAR(TO_DATE(v.cmnt,'DD-MON-YYYY'),'DD-Mon-YYYY') AS cmnt
        FROM v_result_set v, saturn.spriden, saturn.sgbstdn a, saturn.sfrwdrl
       WHERE spriden_pidm = v.pidm
         AND spriden_pidm = a.sgbstdn_pidm
         AND spriden_pidm = sfrwdrl_pidm
         AND sfrwdrl_term_code = v.term_code
         AND spriden_change_ind IS NULL
         ---AND v.pidm IN (364856)		-- DEBUGDEBUG
         AND a.sgbstdn_term_code_eff = (SELECT MAX(aa.sgbstdn_term_code_eff)
                                          FROM saturn.sgbstdn aa
                                         WHERE aa.sgbstdn_pidm = a.sgbstdn_pidm
                                           AND aa.sgbstdn_term_code_eff <= sfrwdrl_term_code)									  
    ORDER BY student_name, shrttcm_term_code DESC;
  
      CURSOR c_shrttcm_del IS
        WITH v_result_set AS (
      SELECT shrttcm_pidm AS pidm,
             shrttcm_term_code AS term_code,
             RTRIM(LTRIM(REPLACE(shrttcm_comment, c_wthdrl))) AS cmnt
        FROM saturn.shrttcm
       WHERE shrttcm_term_code >= c_legacy_term
       MINUS
      SELECT w.sfrwdrl_pidm AS pidm,
             w.sfrwdrl_term_code AS term_code,
             TO_CHAR(w.sfrwdrl_eff_wdrl_date, 'DD-Mon-YYYY') AS cmnt
        FROM saturn.sfrwdrl w
       WHERE w.sfrwdrl_term_code >= c_legacy_term
         AND w.sfrwdrl_seq_no = (SELECT MAX(ww.sfrwdrl_seq_no)
                                   FROM saturn.sfrwdrl ww
                                  WHERE ww.sfrwdrl_pidm = w.sfrwdrl_pidm
                                    AND ww.sfrwdrl_term_code = w.sfrwdrl_term_code)
      )
      SELECT v.pidm AS shrttcm_pidm,
             spriden_id AS dsi,
             INITCAP(SUBSTR(LTRIM(spriden_last_name)||', '||NVL(spriden_first_name,' '),1,30)) AS student_name,
             shrttcm_levl_code AS shrttcm_levl_code,
             shrttcm_term_code AS shrttcm_term_code,
             shrttcm_comment AS shrttcm_comment
        FROM v_result_set v, saturn.shrttcm, saturn.spriden
       WHERE spriden_pidm = v.pidm
         AND shrttcm_pidm = v.pidm
         AND shrttcm_term_code = v.term_code
         AND spriden_change_ind IS NULL    
         ---AND v.pidm IN (364856)		-- DEBUGDEBUG	 
    ORDER BY student_name, shrttcm_term_code DESC;
	
    BEGIN
       v_timing := DBMS_UTILITY.GET_TIME(); 

       DBMS_OUTPUT.ENABLE(NULL);

       SELECT SUBSTR(USER,1,30) INTO v_user_id FROM DUAL;
       
       IF p_user_id IS NOT NULL THEN v_user_id := SUBSTR(p_user_id,1,30); END IF;
	   	   
       OPEN c_shrttcm_del;
       LOOP
          BEGIN
             FETCH c_shrttcm_del INTO v_pidm, v_dsi, v_name, v_levl_code, v_term_code, v_cmnt;
             EXIT WHEN c_shrttcm_del%NOTFOUND;

             v_rec := RPAD(v_dsi,11,' ')||
                      RPAD(v_name,32,' ')||
                      RPAD(v_term_code,8,' ')||
                      RPAD(v_levl_code,14,' ')||
                      RPAD(v_cmnt,32,' ')||
                      RPAD(v_user_id,30,' ');
		  
             DELETE 
               FROM saturn.shrttcm
              WHERE shrttcm_pidm = v_pidm
                AND shrttcm_levl_code = v_levl_code
                AND shrttcm_term_code = v_term_code
                AND shrttcm_comment = v_cmnt;			   

             IF SQL%ROWCOUNT > 0 THEN
                v_idx := v_idx + 1;
                v_ddx := v_ddx + 1;
                log_rec(v_idx).pidm := v_pidm;
                log_rec(v_idx).dsi := v_dsi;
                log_rec(v_idx).student_name := v_name;
                log_rec(v_idx).term_code := v_term_code;
                log_rec(v_idx).levl_code := v_levl_code;
                log_rec(v_idx).cmnt := v_cmnt;
                log_rec(v_idx).msg := 'Delete';
             END IF;         
		  
             EXCEPTION
                WHEN OTHERS THEN
                   v_err_idx := v_err_idx + 1;
                   err_rec(v_err_idx).pidm := v_pidm;
                   err_rec(v_err_idx).dsi := v_dsi;
                   err_rec(v_err_idx).student_name := v_name;
                   err_rec(v_err_idx).levl_code := v_levl_code;
                   err_rec(v_err_idx).term_code := v_term_code;
                   err_rec(v_err_idx).cmnt := v_cmnt;
                   err_rec(v_err_idx).msg := 'Delete';
                   err_rec(v_err_idx).err := ' Delete Error: '||SUBSTR(SQLERRM,1,120);			 
	      END;
       END LOOP;
       CLOSE c_shrttcm_del;

       OPEN c_shrttcm_ins;
       LOOP
          BEGIN
             FETCH c_shrttcm_ins INTO v_pidm, v_dsi, v_name, v_levl_code, v_term_code, v_cmnt;
             EXIT WHEN c_shrttcm_ins%NOTFOUND;
             --IF v_dsi IN ('D40197132') THEN v_term_code := '123456'; END IF;
             --IF v_dsi IN ('D40326850') THEN v_levl_code := 'MD'; END IF;
             v_rec := RPAD(v_dsi,11,' ')||
                      RPAD(v_name,32,' ')||
                      RPAD(v_term_code,8,' ')||
                      RPAD(v_levl_code,14,' ')||
                      RPAD(v_cmnt,32,' ')||
                      RPAD(v_user_id,30,' ');
		  
             MERGE INTO saturn.shrttcm tgt
             USING (SELECT v_pidm AS shrttcm_pidm, 
                           v_levl_code AS shrttcm_levl_code, 
                           v_term_code AS shrttcm_term_code,	
                           v_cmnt AS shrttcm_comment,				   
                           v_user_id AS shrttcm_user_id,
                           c_rundate AS shrttcm_activity_date
                      FROM DUAL) src
                        ON (       tgt.shrttcm_pidm = src.shrttcm_pidm
                               AND tgt.shrttcm_levl_code = src.shrttcm_levl_code
                               AND tgt.shrttcm_term_code = src.shrttcm_term_code
					           AND tgt.shrttcm_comment = src.shrttcm_comment)
              WHEN NOT MATCHED THEN 
                   INSERT(tgt.shrttcm_pidm, 
                          tgt.shrttcm_levl_code, 
                          tgt.shrttcm_term_code,
                          tgt.shrttcm_comment,
                          tgt.shrttcm_user_id,					   
                          tgt.shrttcm_activity_date) 
                   VALUES(src.shrttcm_pidm, 
                          src.shrttcm_levl_code, 
                          src.shrttcm_term_code,
                          src.shrttcm_comment,
                          src.shrttcm_user_id,					   
                          src.shrttcm_activity_date);
             IF SQL%ROWCOUNT > 0 THEN
                v_idx := v_idx + 1;
                log_rec(v_idx).pidm := v_pidm;
                log_rec(v_idx).dsi := v_dsi;
                log_rec(v_idx).student_name := v_name;
                log_rec(v_idx).term_code := v_term_code;
                log_rec(v_idx).levl_code := v_levl_code;
                log_rec(v_idx).cmnt := v_cmnt;
                log_rec(v_idx).msg := 'Insert';
             END IF;         
		  
             EXCEPTION
                WHEN OTHERS THEN
                   v_err_idx := v_err_idx + 1;
                   err_rec(v_err_idx).pidm := v_pidm;
                   err_rec(v_err_idx).dsi := v_dsi;
                   err_rec(v_err_idx).student_name := v_name;
                   err_rec(v_err_idx).levl_code := v_levl_code;
                   err_rec(v_err_idx).term_code := v_term_code;
                   err_rec(v_err_idx).cmnt := v_cmnt;
                   err_rec(v_err_idx).msg := 'Insert';
                   err_rec(v_err_idx).err := ' Insert Error: '||SUBSTR(SQLERRM,1,120);			  
	      END;
       END LOOP;
       CLOSE c_shrttcm_ins;
	   
       p_record_ins := v_idx - v_ddx;
       p_record_del := v_ddx;
       p_error_cnt := v_err_idx;
         		  	   
       IF v_idx > 0 OR v_err_idx > 0 THEN
          DBMS_OUTPUT.NEW_LINE;
          DBMS_OUTPUT.NEW_LINE;
          DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-')); 
          DBMS_OUTPUT.PUT_LINE('Transaction  '||RPAD('Student Name',32,' ')||'Student D#  '||
                     'Term Code  '||'Level Code   '||RPAD('Transcript Comment',32,' '));
          DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-'));			   
          FOR n IN 1 .. v_idx LOOP
             DBMS_OUTPUT.PUT_LINE(RPAD(log_rec(n).msg,13,' ')|| 
                                  RPAD(log_rec(n).student_name,32,' ')||
                                  RPAD(log_rec(n).dsi,12,' ')||                                                          
                                  RPAD(log_rec(n).term_code,11,' ')||
                                  RPAD(log_rec(n).levl_code,13,' ')|| 								  
                                  RPAD(SUBSTR(NVL(log_rec(n).cmnt, 'NULL'),1,30),32,' '));
          END LOOP;
          --DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-'));
       ELSE
          DBMS_OUTPUT.NEW_LINE;
          DBMS_OUTPUT.PUT_LINE(RPAD('No record is processed by '||c_job,c_line_length,'-'));
          DBMS_OUTPUT.NEW_LINE;
       END IF;
   
       IF v_err_idx > 0 THEN
          DBMS_OUTPUT.NEW_LINE;
          DBMS_OUTPUT.PUT_LINE(RPAD('The following records have errors:',c_line_length,'-'));       
          FOR n IN 1 .. v_err_idx LOOP
             DBMS_OUTPUT.PUT_LINE(RPAD(err_rec(n).msg,13,' ')|| 
                                  RPAD(err_rec(n).student_name,32,' ')||
                                  RPAD(err_rec(n).dsi,12,' ')||
                                  RPAD(NVL(err_rec(n).term_code,'NULL'),11,' ')||  								  
                                  RPAD(NVL(err_rec(n).levl_code,'NULL'),13,' ')||                                                  
                                  RPAD(SUBSTR(NVL(err_rec(n).cmnt,'NULL'),1,30),32,' ')||
                                  SUBSTR(err_rec(n).err,1,255));
          END LOOP;
          DBMS_OUTPUT.PUT_LINE(RPAD('-',c_line_length,'-'));   
       END IF;
       DBMS_OUTPUT.NEW_LINE;
				 
       IF v_user_id = 'BANINST1' OR p_debug = TRUE THEN
          v_timing := DBMS_UTILITY.GET_TIME() - v_timing;
          v_timing := ROUND(v_timing/100, 2);
          DBMS_OUTPUT.PUT_LINE(c_job||' Total Time Consumed in seconds:      '||
                               v_timing);
          DBMS_OUTPUT.PUT_LINE(c_job||' Started at:                          '||
		              TO_CHAR(c_rundate, 'DD-Mon-YYYY HH12:MI:SS AM'));	
          DBMS_OUTPUT.PUT_LINE(c_job||' Finished at:                         '||
		              TO_CHAR(SYSDATE,'DD-Mon-YYYY HH12:MI:SS AM'));
          DBMS_OUTPUT.PUT_LINE(c_job||' Total Records Inserted:              '||
                               TO_CHAR(p_record_ins));
          DBMS_OUTPUT.PUT_LINE(c_job||' Total Records Deleted:               '||
                               TO_CHAR(p_record_del));
          DBMS_OUTPUT.PUT_LINE(c_job||' Total Records w Error/Not Processed: '||
                               TO_CHAR(p_error_cnt));	
          DBMS_OUTPUT.NEW_LINE;
       END IF;
       
       EXCEPTION
       WHEN OTHERS THEN          
          DBMS_OUTPUT.PUT_LINE(v_rec||' '||SUBSTR(SQLERRM,1,120));
          ROLLBACK;

END szptlda_exec;
	
-- ***************************************************************************
-- * MAIN PROCEDURE BODY
-- ***************************************************************************
BEGIN

   DBMS_OUTPUT.ENABLE (buffer_size =>NULL);

   /*Check Security*/
   IF f$_check_security('SZPTLDA') THEN NULL;
   ELSE
      dbms_output.put_line('User Not Authorized');
   END IF;
   /*End Security Check*/
   SELECT TO_CHAR(SYSDATE,'DD-Mon-YYYY HH12:MI:SS AM')
   INTO v_start_time
   FROM DUAL;	
   --Get the job parameters
   BEGIN
      SELECT TO_CHAR(TRIM(gjbprun_value)), gjbpdef_desc
        INTO v_runmode, v_runmode_desc
        FROM general.gjbprun, general.gjbpdef
       WHERE gjbprun_job = gjbpdef_job
         AND gjbprun_job = c_job_name
         AND gjbprun_number = gjbpdef_number
         AND gjbprun_one_up_no = v_one_up_number
         AND gjbprun_number = '01';
      EXCEPTION
      WHEN NO_DATA_FOUND THEN
         RAISE e_norunmode;
   END;

   general.p_create_report_header (p_start_time  => SYSDATE,
                                   p_job_name    =>	c_job_name,
                                   p_line_length => c_line_length,
                                   p_line_1      =>	v_header_line_1,
                                   p_line_2      =>	v_header_line_2,
                                   p_error       =>	v_header_error);
   IF v_header_error THEN
      RAISE	e_header_error;
   ELSE
      DBMS_OUTPUT.PUT_LINE (v_header_line_1);
      DBMS_OUTPUT.PUT_LINE (v_header_line_2);
   END IF;	
--
   --p_print_sub_header;   
--
   szptlda_exec(p_user_id      =>  v_user,
                p_debug        =>  FALSE,
                p_record_ins   =>  v_recordins,
				p_record_del   =>  v_recorddel,
                p_error_cnt    =>  v_errorcnt);

   IF v_runmode = 'A' THEN
      baninst1.gb_common.p_rollback;
   ELSIF v_runmode = 'U' AND (v_recordins + v_recorddel) > 0 THEN
      baninst1.gb_common.p_commit;
   END IF;
	
	--Reset security------------
   security_status := f$_check_security('RESET');
	--Add footer to the output
   p_print_footer;	
EXCEPTION
   WHEN e_norunmode THEN
      DBMS_OUTPUT.PUT_LINE(CHR(10));
      DBMS_OUTPUT.PUT_LINE('No run mode provided.');  
      baninst1.gb_common.p_rollback;
      p_print_footer;
--
   WHEN e_header_error THEN
      DBMS_OUTPUT.PUT_LINE(CHR(10));
      DBMS_OUTPUT.PUT_LINE ('v_header_line_1 '||v_header_line_1);
      baninst1.gb_common.p_rollback;
      p_print_footer;					
--
   WHEN OTHERS THEN
      DBMS_OUTPUT.PUT_LINE('SZPTLDA ERROR: '||SUBSTR(SQLERRM,1,200)||' '||TO_CHAR(SQLCODE));
      p_print_footer;
      baninst1.gb_common.p_rollback; 
END ;
/
SPOOL OFF
EXIT
