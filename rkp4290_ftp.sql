/**** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; ****/

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_FTP

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_FTP.SQL                                         **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_FTP                                        **
**                                                                           **
**  AUTHOR   : REDDY S CHILUKA                                               **
**  CREATED  : 27 JANUARY 2000                                               **
**  PURPOSE  :This procedure should be executed from within the VB           **
**            application RKP9290.  This procedure is to create a file in    **
**            the appropriate tmp directory which will be used to invoke     **
**            RKP4291 or RKP4292 program.                                    **
**                                                                           **
** id_means_of_transp is named as trans_id                                   **
** ************************************************************************* */
(
   p_entry_no           IN    ENTRY_HDR.ENTRY_NO%TYPE,
   p_file_name          IN    VARCHAR2,
   p_user_id            IN    VARCHAR2,
   p_fname              IN    VARCHAR2,
   p_file_status        OUT   VARCHAR2
)
AS
     v_suffix                VARCHAR2(20);
     v_instance              VARCHAR2(20);
     v_user                  ENTRY_HDR.LAST_USER%TYPE;
     p3                      NUMBER;

-- File variables

     file_path               VARCHAR2(75);
     file_name               VARCHAR2(50);
     file_handle             UTL_FILE.FILE_TYPE;

--Buffer to holp output to be written to file

     outbuffer               VARCHAR2(30);

BEGIN
  v_user        := USER;
  p_file_status := 'TRUE';

  BEGIN
    SELECT SUBSTR(GLOBAL_NAME, 1, 5) INTO v_instance FROM GLOBAL_NAME;

    IF v_instance = 'ROCKD' THEN
        file_path := '/apps/ana/dev/rockport/rockd/data/ftp/expeditors/receive/tmp';
    ELSIF v_instance = 'ROCKI' THEN
        file_path := '/apps/ana/dev/rockport/rocki/data/ftp/expeditors/receive/tmp';
    ELSIF v_instance = 'ROCKP' THEN
        file_path := '/apps/ana/prod/rockport/rockp/data/ftp/expeditors/receive/tmp';
    ELSIF v_instance = 'ROCKQ' THEN
        file_path := '/apps/ana/qa/rockport/rockq/data/ftp/expeditors/receive/tmp';
    ELSIF v_instance = 'ROCKS' THEN
        file_path := '/apps/ana/dev/rockport/rocks/data/ftp/expeditors/receive/tmp';
    ELSIF v_instance = 'ROCKT' THEN
        file_path := '/apps/ana/dev/rockport/rockt/data/ftp/expeditors/receive/tmp';
    ELSE
        p_file_status := 'FALSE';

        p3 := shrmsglog_pkg.snd_log_msg
               ( 'T_SP_RKP4290_FTP',
                 p_entry_no,
                 '0001',
                 'IF',
                 USER,
                 'QQ',
                 TO_CHAR(SQLCODE),
                 'ERR',
                 SQLERRM,
                 'INVALID DATABASE INSTANCE',
                 p_file_name||' user_id '||p_entry_no,
                 'Cannot determine the instance, the file needs to be created.',
                 v_user
               );
    END IF;

    EXCEPTION
      WHEN OTHERS THEN
        p_file_status := 'FALSE';

        p3 := shrmsglog_pkg.snd_log_msg
               ( 'T_SP_RKP4290_FTP',
                   p_entry_no,
                   '0002',
                   'IF',
                   USER,
                   'QQ',
                   TO_CHAR(SQLCODE),
                   'ERR',
                   SQLERRM,
                   'INVALID DATABASE INSTANCE',
                   p_file_name||' user_id '||p_entry_no,
                   'Select on GLOBAL_NAME failed and instance is not determined',
                   v_user
               );
  END;

  SELECT TO_CHAR(SYSDATE, 'YYYYMMDDHH24MISSSSS')
    INTO v_suffix
    FROM DUAL;

  file_name := p_file_name || LTRIM(RTRIM(p_entry_no));

  file_name := file_name || '.' || LTRIM(RTRIM(v_suffix));

  IF p_file_name = 'RKP4291_' THEN
      outbuffer := LTRIM(RTRIM(p_entry_no));
  ELSIF p_file_name = 'RKP4292_' THEN
      outbuffer := p_entry_no || ' ' || LTRIM(RTRIM(P_USER_ID))|| ' '||LTRIM(RTRIM(P_fname));
  END IF;

  file_handle := UTL_FILE.FOPEN(file_path, file_name, 'w');

  UTL_FILE.PUT_LINE(file_handle, outbuffer);

  UTL_FILE.FCLOSE(file_handle);

  EXCEPTION
    WHEN UTL_FILE.INVALID_FILEHANDLE THEN
      p_file_status := 'FALSE';

      p3 := shrmsglog_pkg.snd_log_msg
             ( 'T_SP_RKP4290_FTP',
               p_entry_no,
               '0003',
               'UTL_FILE',
               USER,
               'QQ',
               TO_CHAR(SQLCODE),
               'ERR',
               SQLERRM,
               'INVALID FILE PATH',
               p_file_name||' user_id '||p_entry_no,
               'If file not created, contact person on call',
               v_user
             );

    WHEN UTL_FILE.INVALID_PATH THEN
      p_file_status := 'FALSE';

      p3 := shrmsglog_pkg.snd_log_msg
             ( 'T_SP_RKP4290_FTP',
               p_entry_no,
               '0004',
               'UTL_FILE',
               USER,
               'QQ',
               TO_CHAR(SQLCODE),
               'ERR',
               SQLERRM,
               'INVALID FILE PATH',
               p_file_name||' user_id '||p_entry_no,
               'If file not created, contact person on call',
               v_user
             );

    WHEN UTL_FILE.INVALID_MODE THEN
      p_file_status := 'FALSE';

      p3 := shrmsglog_pkg.snd_log_msg
             ( 'T_SP_RKP4290_FTP',
               p_entry_no,
               '0005',
               'UTL_FILE',
               USER,
               'QQ',
               TO_CHAR(SQLCODE),
               'ERR',
               SQLERRM,
               'INVALID FILE MODE',
               p_file_name||' user_id '||p_entry_no,
               'If file not created, contact person on call',
               v_user
             );

    WHEN UTL_FILE.INTERNAL_ERROR THEN
      p_file_status := 'FALSE';

      p3 := shrmsglog_pkg.snd_log_msg
             ( 'T_SP_RKP4290_FTP',
               p_entry_no,
               '0006',
               'UTL_FILE',
               USER,
               'QQ',
               TO_CHAR(SQLCODE),
               'ERR',
               SQLERRM,
               'INTERNAL ERROR',
               p_file_name||' user_id '||p_entry_no,
               'If file not created, contact person on call',
               v_user
             );

    WHEN UTL_FILE.WRITE_ERROR THEN
      p_file_status := 'FALSE';

      p3 := shrmsglog_pkg.snd_log_msg
             ( 'T_SP_RKP4290_FTP',
               p_entry_no,
               '0007',
               'UTL_FILE',
               USER,
               'QQ',
               TO_CHAR(SQLCODE),
               'ERR',
               SQLERRM,
               'INVALID FILE MODE',
               p_file_name||' user_id '||p_entry_no,
               'If file not created, contact person on call',
               v_user
             );

END T_SP_RKP4290_FTP;
/
EXIT
