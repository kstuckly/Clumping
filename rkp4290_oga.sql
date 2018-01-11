
/* *** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; *** */

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_OGA

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_OGA.SQL                                         **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_OGA                                        **
**                                                                           **
**  AUTHOR   : KAVIN J STUCKLY                                               **
**  CREATED  : 06 DECEMBER 2000                                              **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedure T_SP_RKP4290.  This procedure is to validate        **
**             OGA (Other Government Agencies) information.                  **
**                                                                           **
** ************************************************************************* */
(
   p_table                IN     VARCHAR2,
   p_owner                IN     ENTRY_INV.OWNER%TYPE,
   p_entry_no             IN     ENTRY_HDR.ENTRY_NO%TYPE,
   p_bill_of_lading_no    IN     ENTRY_INV.BILL_OF_LADING_NO%TYPE,
   p_import_date          IN     ENTRY_HDR.IMPORT_DATE%TYPE,
   p_export_country       IN     PRECLASS_DTL.EXPORT_COUNTRY%TYPE,
   p_item                 IN     ENTRY_DTL.ITEM%TYPE,
   p_hts_no               IN     ENTRY_DTL.HTS_NO%TYPE,
   p_hts_line_no          IN     PRECLASS_DTL.LINE_NO%TYPE,
   p_nonchargeable_sample IN     VARCHAR2,
   p_origin_country       IN     ENTRY_DTL.ORIGIN_COUNTRY%TYPE,
   p_med_check            IN OUT VARCHAR2,
   p_load_status             OUT VARCHAR2
)

AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   c_unit_1               HTS.UNIT_1%TYPE;
   c_unit_2               HTS.UNIT_2%TYPE;
   c_unit_3               HTS.UNIT_3%TYPE;

   c_oga_1                HTS.OGA_1%TYPE;
   c_oga_2                HTS.OGA_2%TYPE;
   c_oga_3                HTS.OGA_3%TYPE;
   c_oga_4                HTS.OGA_4%TYPE;
   c_oga_5                HTS.OGA_5%TYPE;

   c_link_tbl_name        SPECCOND.LINK_TBL_NAME%TYPE;
   c_link_key_val         SPECCOND.LINK_KEY_VAL%TYPE;
   c_doc_msg_code         SPECCOND.DOC_MSG_CODE%TYPE;
   c_count                NUMBER(5);
   p3                     NUMBER;



   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */

   g_doc_msg_code_ac1     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AC1';
   g_doc_msg_code_ac2     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AC2';
   g_doc_msg_code_ac3     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AC3';
   g_doc_msg_code_ac4     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AC4';
   g_doc_msg_code_aq1     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AQ1';
   g_doc_msg_code_aq2     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AQ2';
   g_doc_msg_code_aq3     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AQ3';
   g_doc_msg_code_aq4     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'AQ4';

   g_doc_msg_code_doc     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'DOC';
   g_doc_msg_code_dod     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'DOD';
   g_doc_msg_code_dot     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'DOT';

   g_doc_msg_code_fca     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCA';
   g_doc_msg_code_fcc     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCC';
   g_doc_msg_code_fc1     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FC1';
   g_doc_msg_code_fc2     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FC2';
   g_doc_msg_code_fc3     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FC3';
   g_doc_msg_code_fcd     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCD';
   g_doc_msg_code_fcn     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCN';
   g_doc_msg_code_fcr     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCR';
   g_doc_msg_code_fcv     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FCV';
   g_doc_msg_code_fdd     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'FDD';

   g_doc_msg_code_med     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'MED';
   g_doc_msg_code_mtc     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'MTC';
   g_doc_msg_code_pdc     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'PDC';
   g_doc_msg_code_vnf     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE  := 'VNF';

   g_speccond_ltn         CONSTANT SPECCOND.LINK_TBL_NAME%TYPE := 'PRECLASS - ';
   g_speccond_item_ltn    CONSTANT SPECCOND.LINK_TBL_NAME%TYPE := 'ITEM - ';
   g_hts_ctry_code_us     CONSTANT HTS.COUNTRY_CODE%TYPE       := 'US';   

   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */


   SKIP_OGA_CHECKS        EXCEPTION;
   OGA_CHECKS_ERR         EXCEPTION;

/* ************************************************************************* */


/* ************************************************************************* **
**           BEGIN OF LOCAL FUNCTION SET_LOAD_STATUS                         **

             Load Status translation for end users
               ERROR - Shipments Clumping was unsuccessful (Contact RSTS)
               FALSE - Shipments Clumping was unsuccessful (Check error rpt)
               WARN  - Shipments Clumped with Warnings
               TRUE  - Shipments Clumped Successfully
** ************************************************************************* */

FUNCTION  SET_LOAD_STATUS
   (
    l_status            IN  VARCHAR2
   )
   RETURN VARCHAR2

IS

BEGIN
  IF l_status NOT IN ('ERROR', 'FALSE', 'WARN', 'TRUE') THEN
      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_OGA', p_entry_no, '201', 'IF', USER,
             'CE', '1', 'ERR', NULL,
             'Internal Stored Procedure Error',
             'Load status of ' || l_status || ' is invalid',
             'Load status must be WARN, FALSE, TRUE or ERROR',
             NULL
            );

      RETURN('ERROR'); 
  END IF;


  IF l_status = 'ERROR' THEN
      RETURN(l_status);

  ELSIF l_status = 'FALSE' THEN
      IF p_load_status = 'ERROR' THEN
          RETURN(p_load_status);
      ELSE
          RETURN(l_status);
      END IF;

  ELSIF l_status = 'WARN' THEN
      IF p_load_status in ('ERROR', 'FALSE') THEN
          RETURN(p_load_status);
      ELSE
          RETURN(l_status);
      END IF;

  ELSE
      IF p_load_status in ('ERROR', 'FALSE', 'WARN') THEN
          RETURN(p_load_status);
      ELSE
          RETURN(l_status);
      END IF;
  END IF;

END; 

/* ************************************************************************* **
             END OF LOCAL FUNCTION SET_LOAD_STATUS             
** ************************************************************************* */


/* ************************************************************************* **
**           BEGIN OF LOCAL STORED PROCEDURE MEDIA_CHECK                     **
** ************************************************************************* */

PROCEDURE MEDIA_CHECK IS

BEGIN
  IF (c_unit_1 IN ('LNM', 'FBM', 'M2') OR
      c_unit_2 IN ('LNM', 'FBM', 'M2') OR
      c_unit_3 IN ('LNM', 'FBM', 'M2')) THEN

       c_count := 0;

       IF p_med_check = 'FALSE' THEN
           if p_nonchargeable_sample = 'TRUE' and p_table != 'ITEM' THEN
               BEGIN
                 SELECT COUNT(1) INTO c_count
                   FROM SPECCOND
                  WHERE OWNER         = p_owner
                    AND LINK_TBL_NAME = g_speccond_ltn || LTRIM(RTRIM(p_item))
                    AND LINK_KEY_VAL  = LTRIM(RTRIM(p_item)) || ' + ' ||
                                           LTRIM(RTRIM(p_item)) || ' + ' ||
                                           p_origin_country || ' + 1'

                    AND DOC_MSG_CODE  = g_doc_msg_code_med;

                 IF c_count = 0 THEN
                     p_load_status := SET_LOAD_STATUS('WARN');

                     p3 := shrmsglog_pkg.snd_log_msg
                           ('T_SP_RKP4290_OGA', p_entry_no, '202', 'IF',
                            USER, 'CW', '1', 'INF', NULL,
                            p_table || ' '|| p_item,
                            'MED Cond/Doc indicator for Sample is missing.',
                            'Docs/Cond Booster should be updated.', NULL
                           );
                 END IF;

                 EXCEPTION
                   WHEN OTHERS THEN
                     p_load_status := 'ERROR';

                     p3 := shrmsglog_pkg.snd_log_msg
                           ('T_SP_RKP4290_OGA', p_entry_no, '203', 'SELECT',
                            USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                            p_table || ' '|| p_item,
                            'Error Select MED Cond/Doc Booster for Sample',
                            NULL, NULL
                           );

                     RAISE OGA_CHECKS_ERR;
               END; 

           ELSE
               BEGIN
                 SELECT COUNT(1) INTO c_count
                   FROM SPECCOND
                  WHERE OWNER         = p_owner
                    AND LINK_TBL_NAME = g_speccond_item_ltn || LTRIM(RTRIM(p_item))
                    AND LINK_KEY_VAL  = LTRIM(RTRIM(p_item))
                    AND DOC_MSG_CODE  = g_doc_msg_code_med;

                 IF c_count = 0 THEN
                     p_load_status := SET_LOAD_STATUS('WARN');

                     p3 := shrmsglog_pkg.snd_log_msg
                           ('T_SP_RKP4290_OGA', p_entry_no, '204', 'IF',
                            USER, 'CW', '1', 'INF', NULL,
                            'Item  '|| p_item,
                            'MED Cond/Doc indicator off Item is missing',
                            'Docs/Cond Booster should be updated.', NULL
                           );
                 END IF;

                 EXCEPTION
                   WHEN OTHERS THEN
                     p_load_status := 'ERROR';

                     p3 := shrmsglog_pkg.snd_log_msg
                           ('T_SP_RKP4290_OGA', p_entry_no, '205',
                            'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                            SQLERRM,
                            p_table || ' '|| p_item,
                            'Error Select MED Cond/Doc Booster off Item',
                            NULL, NULL
                           );

                     RAISE OGA_CHECKS_ERR;
               END; 

           END IF;

           p_med_check := 'TRUE';

       END IF;

       c_count := 0;

       BEGIN
         SELECT COUNT(1) INTO c_count
           FROM SPECCOND
          WHERE OWNER         = p_owner
            AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
            AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
            AND DOC_MSG_CODE  = g_doc_msg_code_mtc;

         EXCEPTION
           WHEN OTHERS THEN
             p_load_status := 'ERROR';

             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_OGA', p_entry_no, '206',
                    'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                    SQLERRM,
                    p_table || ' '|| p_item,
                    'Ltn ' || c_link_tbl_name,
                    'Lkv ' || c_link_key_val,
                    'Error Selecting MTC Cond/Doc Booster'
                   );

             RAISE OGA_CHECKS_ERR;

       END;  

       IF c_count = 0 THEN
           p_load_status := SET_LOAD_STATUS('WARN');

           p3 := shrmsglog_pkg.snd_log_msg
                 ('T_SP_RKP4290_OGA', p_entry_no, '207', 'IF',
                  USER, 'CW', '1', 'INF', NULL,
                  p_table || ' ' || p_item,    
                  'MTC Cond/Doc indicator is missing.',
                  'Docs/Cond Booster should be updated.', NULL
                  );
       END IF;
   END IF;
END;

/* ************************************************************************* **
**           END OF LOCAL STORED PROCEDURE MEDIA_CHECK                       **
** ************************************************************************* */


/* ************************************************************************* **
**           BEGIN OF LOCAL STORED PROCEDURE FCC_CHECK                       **
** ************************************************************************* */

PROCEDURE FCC_CHECK IS

BEGIN
  c_count := 0;

  IF (c_oga_1 LIKE 'FC%' OR c_oga_2 LIKE 'FC%' OR
      c_oga_3 LIKE 'FC%' OR c_oga_4 LIKE 'FC%' OR
      c_oga_5 LIKE 'FC%') THEN

      BEGIN
        SELECT COUNT(1) INTO c_count
          FROM SPECCOND
         WHERE OWNER         = p_owner
           AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
           AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
           AND DOC_MSG_CODE  = g_doc_msg_code_fcd;

	EXCEPTION
          WHEN OTHERS THEN
            p_load_status := 'ERROR';

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290_OGA', p_entry_no, '208',
                   'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                   SQLERRM,
                   'Ltn ' || c_link_tbl_name,
                   'Lkv ' || c_link_key_val,
                   'Error Selecting FCD Cond/Doc Booster',
                   NULL
                  );

            RAISE OGA_CHECKS_ERR;
      END; 

      IF c_count > 0 AND (c_oga_1 = 'FC4' OR
                          c_oga_2 = 'FC4' OR c_oga_3 = 'FC4' OR
                          c_oga_4 = 'FC4' OR c_oga_5 = 'FC4'
                         ) THEN

          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_OGA', p_entry_no, '209', 'IF',
                 USER, 'CW', '1', 'INF', NULL,
                 'Item '|| p_item,
                 'HTS# requires FCC information, FCC disclaimer ' ||
                      'is indicated.',
                 'Update FCC information in the Docs/Cond Booster', NULL
                );

      ELSIF c_count = 0 THEN
          BEGIN
            SELECT COUNT(1) INTO c_count
              FROM SPECCOND
             WHERE OWNER         = p_owner
               AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
               AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
               AND DOC_MSG_CODE  IN (g_doc_msg_code_fcc,
                                     g_doc_msg_code_fca,
                                     g_doc_msg_code_fcn);

            EXCEPTION
              WHEN OTHERS THEN
                p_load_status := 'ERROR';
    
                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290_OGA', p_entry_no, '210',
                       'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                       SQLERRM,
                       'Ltn ' || c_link_tbl_name,
                       'Lkv ' || c_link_key_val,
                       'Error Selecting FCC, FCA, FCN Cond/Doc Boosters',
                       NULL
                      );

                RAISE OGA_CHECKS_ERR;
          END; 

          IF c_count > 0 THEN
              c_count := 0;

              BEGIN
                SELECT COUNT(1) INTO c_count
                  FROM SPECCOND
                 WHERE OWNER         = p_owner
                   AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                   AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                   AND DOC_MSG_CODE  IN (g_doc_msg_code_fc1,
                                         g_doc_msg_code_fc2, 
                                         g_doc_msg_code_fc3);

                EXCEPTION
                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';
    
                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290_OGA', p_entry_no, '211',
                           'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                           SQLERRM,
                           'Ltn ' || c_link_tbl_name,
                           'Lkv ' || c_link_key_val,
                           'Error Selecting FC1, FC2, FC3 Cond/Doc Boosters',
                           NULL
                          );

                    RAISE OGA_CHECKS_ERR;
              END; 

              IF c_count = 0 THEN
                  p_load_status := SET_LOAD_STATUS('WARN');

                  p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290_OGA', p_entry_no, '212', 'IF',
                         USER, 'CW', '1', 'INF', NULL,
                         'Item '|| p_item,
                         'FCC identifier is missing in the '||
                            'Docs/Cond Booster',
                         NULL, NULL
                        );
              END IF;

          ELSE
              c_count := 0;

              BEGIN
                SELECT COUNT(1) INTO c_count
                  FROM SPECCOND
                 WHERE OWNER         = p_owner
                   AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                   AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                   AND DOC_MSG_CODE  IN (g_doc_msg_code_doc,
                                         g_doc_msg_code_fcv, 
                                         g_doc_msg_code_fcr);

                EXCEPTION
                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';
    
                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290_OGA', p_entry_no, '213',
                           'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                           SQLERRM,
                           'Ltn ' || c_link_tbl_name,
                           'Lkv ' || c_link_key_val,
                           'Error Selecting DOC, FCV, FCR Cond/Doc Boosters',
                           NULL
                          );

                    RAISE OGA_CHECKS_ERR;
              END; 

              IF (c_count = 0) THEN
                  p_load_status := SET_LOAD_STATUS('WARN');
   
                  p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290_OGA', p_entry_no, '214', 'IF', 
                         USER, 'CW', '1', 'INF', NULL,
                         'Item '|| p_item, 
                         'FCC Identifiers not found ' ||
                                'in OGA Docs/Cond Booster.',
                         NULL, NULL
                        );
              END IF;
          END IF;
      END IF;
  END IF;  
END; 

/* ************************************************************************* **
**           END OF LOCAL STORED PROCEDURE FCC_CHECK                         **
** ************************************************************************* */


/* ************************************************************************* **
**           BEGIN OF LOCAL STORED PROCEDURE FDA_CHECK                       **
** ************************************************************************* */

PROCEDURE FDA_CHECK IS

BEGIN
  c_count := 0;

  IF (c_oga_1 != 'FD0' AND c_oga_2 != 'FD0' AND c_oga_3 != 'FD0'
      AND c_oga_4 != 'FD0' AND c_oga_5 != 'FD0') THEN

       IF (c_oga_1 LIKE 'FD%' OR c_oga_2 LIKE 'FD%' OR c_oga_3 LIKE 'FD%'
          OR c_oga_4 LIKE 'FD%' OR c_oga_5 LIKE 'FD%') THEN

           c_count := 0;

           BEGIN
             SELECT COUNT(1) INTO c_count
               FROM SPECCOND
              WHERE OWNER         = p_owner
                AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                AND DOC_MSG_CODE  = g_doc_msg_code_fdd;

             EXCEPTION
               WHEN OTHERS THEN
                 p_load_status := 'ERROR';
    
                 p3 := shrmsglog_pkg.snd_log_msg
                       ('T_SP_RKP4290_OGA', p_entry_no, '215',
                        'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                        SQLERRM,
                        'Ltn ' || c_link_tbl_name,
                        'Lkv ' || c_link_key_val,
                        'Error Selecting FDD Cond/Doc Boosters',
                        NULL
                       );

                 RAISE OGA_CHECKS_ERR;
           END; 
 
           IF c_count > 0 THEN
               IF (c_oga_1 = 'FD2' OR c_oga_2 = 'FD2' OR c_oga_3 = 'FD2'
                   OR c_oga_4 = 'FD2' OR c_oga_5 = 'FD2') THEN
                    p_load_status := SET_LOAD_STATUS('WARN');
 
                    p3 := shrmsglog_pkg.snd_log_msg
                         ('T_SP_RKP4290_OGA', p_entry_no, '216', 'IF', USER,
                         'CW', '1', 'INF', NULL,
                         'Item '|| p_item,
                         'HTS # requires FDA information, FDA ' || 
                               'disclaimer is indicated.',
                         'Update FDA information in the Docs/Cond Booster.',
                         NULL
                        );
               END IF;
 
           ELSE
               c_count := 0;

               BEGIN
                 SELECT COUNT(1) INTO c_count
                   FROM SPECCOND
                  WHERE OWNER         = p_owner
                    AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                    AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                    AND DOC_MSG_CODE  IN (g_doc_msg_code_vnf, g_doc_msg_code_pdc);

		 EXCEPTION
                   WHEN OTHERS THEN
                     p_load_status := 'ERROR';
    
                     p3 := shrmsglog_pkg.snd_log_msg
                           ('T_SP_RKP4290_OGA', p_entry_no, '217',
                            'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                            SQLERRM,
                            'Ltn ' || c_link_tbl_name,
                            'Lkv ' || c_link_key_val,
                            'Error Selecting VNF, PDC Cond/Doc Boosters',
                            NULL
                           );

                     RAISE OGA_CHECKS_ERR;
               END; 

               IF c_count = 0 THEN
                   p_load_status := SET_LOAD_STATUS('WARN');
 
                   p3 := shrmsglog_pkg.snd_log_msg
                         ('T_SP_RKP4290_OGA', p_entry_no, '218', 'IF',
                          USER, 'CW', '1', 'INF', NULL,
                          'Item '|| p_item,
                          'FDA information is missing in Docs/Cond Booster',
                          NULL, NULL
                         );

               ELSIF c_count = 1 THEN
                   p_load_status := SET_LOAD_STATUS('WARN');
 
                   p3 := shrmsglog_pkg.snd_log_msg
                         ('T_SP_RKP4290_OGA', p_entry_no, '219', 'IF', USER,
                          'CW', '1', 'INF', NULL,
                          'Item '|| p_item,
                          'FDA Product Code or Actual Manufacturer (VNF)',
                          'of product is missing in OGA Docs/Cond Booster.',
                          NULL
                         );
 
               ELSIF c_count = 2 THEN
                   c_count := 0;
 
                   BEGIN
                     SELECT COUNT(1) INTO c_count
                       FROM SPECCOND
                      WHERE OWNER         = p_owner
                        AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                        AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                        AND DOC_MSG_CODE  IN (g_doc_msg_code_ac1, g_doc_msg_code_aq1);
   
                     EXCEPTION
                       WHEN OTHERS THEN
                         p_load_status := 'ERROR';
    
                         p3 := shrmsglog_pkg.snd_log_msg
                               ('T_SP_RKP4290_OGA', p_entry_no, '220',
                                'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                                SQLERRM,
                                'Ltn ' || c_link_tbl_name,
                                'Lkv ' || c_link_key_val,
                                'Error Selecting AC1, AQ1 Cond/Doc Boosters',
                                NULL
                               );

                         RAISE OGA_CHECKS_ERR;
                   END; 

                   IF c_count = 1 THEN
                       p_load_status := SET_LOAD_STATUS('WARN');
   
                       p3 := shrmsglog_pkg.snd_log_msg
                             ('T_SP_RKP4290_OGA', p_entry_no, '221', 'IF',
                              USER, 'CW', '1', 'INF', NULL,
                              'Item '|| p_item,
                              'Both codes (AC1) and qualifiers (AQ1) '||
                                     'are required. One is missing.',
                              'Update the OGA Docs/Cond Booster.', NULL
                              );
                   END IF;
 
                   c_count := 0;
 
                   BEGIN
                     SELECT COUNT(1) INTO c_count
                       FROM SPECCOND
                      WHERE OWNER         = p_owner
                        AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                        AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                        AND DOC_MSG_CODE  IN (g_doc_msg_code_ac2, g_doc_msg_code_aq2);
    
                     EXCEPTION
                       WHEN OTHERS THEN
                         p_load_status := 'ERROR';
    
                         p3 := shrmsglog_pkg.snd_log_msg
                               ('T_SP_RKP4290_OGA', p_entry_no, '222',
                                'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                                SQLERRM,
                                'Ltn ' || c_link_tbl_name,
                                'Lkv ' || c_link_key_val,
                                'Error Selecting AC2, AQ2 Cond/Doc Boosters',
                                NULL
                               );

                         RAISE OGA_CHECKS_ERR;
                   END; 

                   IF c_count = 1 THEN
                       p_load_status := SET_LOAD_STATUS('WARN');
   
                       p3 := shrmsglog_pkg.snd_log_msg
                             ('T_SP_RKP4290_OGA', p_entry_no, '223', 'IF',
                              USER, 'CW', '1', 'INF', NULL,
                              'Item '|| p_item,
                              'Both codes (AC2) and qualifiers (AQ2) ' ||
                                   'are required.  One is missing.',
                              'Update the OGA Docs/Cond Booster.', NULL
                              );
                   END IF;
  
                   c_count := 0;

                   BEGIN
                     SELECT COUNT(1) INTO c_count
                       FROM SPECCOND
                      WHERE OWNER         = p_owner
                        AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                        AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                        AND DOC_MSG_CODE  IN (g_doc_msg_code_ac3, g_doc_msg_code_aq3);
    
                     EXCEPTION
                       WHEN OTHERS THEN
                         p_load_status := 'ERROR';
    
                         p3 := shrmsglog_pkg.snd_log_msg
                               ('T_SP_RKP4290_OGA', p_entry_no, '224',
                                'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                                SQLERRM,
                                'Ltn ' || c_link_tbl_name,
                                'Lkv ' || c_link_key_val,
                                'Error Selecting AC3, AQ3 Cond/Doc Boosters',
                                NULL
                               );

                         RAISE OGA_CHECKS_ERR;
		   END; 

  
                   IF c_count = 1 THEN
                       p_load_status := SET_LOAD_STATUS('WARN');
 
                       p3 := shrmsglog_pkg.snd_log_msg
                             ('T_SP_RKP4290_OGA', p_entry_no, '225', 'IF',
                              USER, 'CW', '1', 'INF', NULL,
                              'Item '|| p_item, 
                              'Both codes (AC3) and qualifiers (AQ3) '||
                                     'are required.  One is missing.',
                              'Update the OGA Docs/Cond Booster.', NULL
                              );
                   END IF;
 
                   c_count := 0;
 
                   BEGIN
                     SELECT COUNT(1) INTO c_count
                       FROM SPECCOND
                      WHERE OWNER         = p_owner
                        AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                        AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                        AND DOC_MSG_CODE  IN (g_doc_msg_code_ac4, g_doc_msg_code_aq4);
    
                     EXCEPTION
                       WHEN OTHERS THEN
                         p_load_status := 'ERROR';
    
                         p3 := shrmsglog_pkg.snd_log_msg
                               ('T_SP_RKP4290_OGA', p_entry_no, '226',
                                'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                                SQLERRM,
                                'Ltn ' || c_link_tbl_name,
                                'Lkv ' || c_link_key_val,
                                'Error Selecting AC4, AQ4 Cond/Doc Boosters',
                                NULL
                               );

                         RAISE OGA_CHECKS_ERR;
                   END; 
    
                   IF c_count = 1 THEN
                       p_load_status := SET_LOAD_STATUS('WARN');
  
                       p3 := shrmsglog_pkg.snd_log_msg
                             ('T_SP_RKP4290_OGA', p_entry_no, '227', 'IF',
                              USER, 'CW', '1', 'INF', NULL,
                              'Item '|| p_item, 'Both codes (AC4) and ' ||
                                    'qualifiers (AQ4) are required.  ' ||
                                    'One is missing.',
                              'Update the OGA Docs/Cond Booster.', NULL
                             );
                   END IF;
               END IF;
           END IF;
      END IF;
  END IF;
END;

/* ************************************************************************* **
**           END OF LOCAL STORED PROCEDURE FDA_CHECK                         **
** ************************************************************************* */


/* ************************************************************************* **
**           BEGIN OF LOCAL STORED PROCEDURE DOT_CHECK                       **
** ************************************************************************* */

PROCEDURE DOT_CHECK IS

BEGIN
  c_count := 0;

  IF (c_oga_1 LIKE 'DT%' OR c_oga_2 LIKE 'DT%' OR c_oga_3 LIKE 'DT%' OR
      c_oga_4 LIKE 'DT%' OR c_oga_5 LIKE 'DT%') THEN

       BEGIN
         SELECT COUNT(1) INTO c_count
           FROM SPECCOND
          WHERE OWNER         = p_owner
            AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
            AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
            AND DOC_MSG_CODE  IN (g_doc_msg_code_dod, g_doc_msg_code_dot);

         EXCEPTION
           WHEN OTHERS THEN
             p_load_status := 'ERROR';
    
             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_OGA', p_entry_no, '228',
                    'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                     SQLERRM,
                     'Ltn ' || c_link_tbl_name,
                     'Lkv ' || c_link_key_val,
                     'Error Selecting DOD, DOT Cond/Doc Boosters',
                     NULL
                    );

             RAISE OGA_CHECKS_ERR;
       END; 

       IF c_count = 1 THEN
           c_doc_msg_code := NULL;

	   BEGIN
             SELECT DOC_MSG_CODE INTO c_doc_msg_code
               FROM SPECCOND
              WHERE OWNER         = p_owner
                AND LINK_TBL_NAME = LTRIM(RTRIM(c_link_tbl_name))
                AND LINK_KEY_VAL  = LTRIM(RTRIM(c_link_key_val))
                AND DOC_MSG_CODE  IN (g_doc_msg_code_dod, g_doc_msg_code_dot);

             EXCEPTION
               WHEN OTHERS THEN
                 p_load_status := 'ERROR';
    
                 p3 := shrmsglog_pkg.snd_log_msg
                       ('T_SP_RKP4290_OGA', p_entry_no, '229',
                        'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                         SQLERRM,
                         'Ltn ' || c_link_tbl_name,
                         'Lkv ' || c_link_key_val,
                         'Error Selecting DOD, DOT Cond/Doc Boosters',
                         NULL
                        );

                 RAISE OGA_CHECKS_ERR;
           END; 

           IF (c_doc_msg_code = 'DOD' AND (c_oga_1 = 'DT2' OR
                                           c_oga_2 = 'DT2' OR
                                           c_oga_3 = 'DT2' OR
                                           c_oga_4 = 'DT2' OR
                                           c_oga_5 = 'DT2')) THEN
               
               p_load_status := SET_LOAD_STATUS('WARN');

               p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290_OGA', p_entry_no, '230', 'IF',
                      USER, 'CW', '1', 'INF', NULL,
                      'Item '|| p_item,
                      'HTS# requires DOT information.  DOT disclaimer ' ||
                         'is found.',
                      'Update the OGA Docs/Cond Booster.', NULL
                     );
           END IF;

        ELSIF c_count != 1 THEN
           p_load_status := SET_LOAD_STATUS('WARN');

           p3 := shrmsglog_pkg.snd_log_msg
                 ('T_SP_RKP4290_OGA', p_entry_no, '231', 'IF',
                  USER, 'CW', '1', 'INF', NULL, 'Item '|| p_item,
                  'HTS# requires DOT information. ' ||
                     'DOT information is invalid or does not exist.',
                  'Update OGA in Docs/Cond Booster.', NULL
                 );

        END IF;
   END IF; 
END; 

/* ************************************************************************* **
**           END OF LOCAL STORED PROCEDURE DOT_CHECK                         **
** ************************************************************************* */


/* ************************************************************************* **
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_OGA           **
** ************************************************************************* */

BEGIN
  c_count := 0;
  p_load_status := 'TRUE';

  
  IF LTRIM(RTRIM(p_hts_no)) IS NULL OR RTRIM(p_hts_no) = 'MULTI' THEN
      RAISE SKIP_OGA_CHECKS;
  END IF;

  c_link_tbl_name := p_table||' - '||LTRIM(RTRIM(p_item));

  IF (p_table = 'ITEM') THEN
      c_link_key_val := LTRIM(RTRIM(p_item));
  ELSE
      c_link_key_val:=LTRIM(RTRIM(p_item))||' + '||LTRIM(RTRIM(p_item));
      c_link_key_val:=c_link_key_val||' + '||LTRIM(RTRIM(p_export_country));
      c_link_key_val:=c_link_key_val ||' + '|| LTRIM(RTRIM(p_hts_line_no));
  END IF;


  BEGIN
    SELECT NVL(UNIT_1, 'X'),  NVL(UNIT_2, 'X'),  NVL(UNIT_3, 'X'),
           NVL(OGA_1,  'X'),  NVL(OGA_2,  'X'),  NVL(OGA_3,  'X'),
           NVL(OGA_4,  'X'),  NVL(OGA_5,  'X')

      INTO c_unit_1, c_unit_2, c_unit_3, c_oga_1, c_oga_2, c_oga_3,
           c_oga_4,  c_oga_5

      FROM HTS

     WHERE HTS_NO           = p_hts_no
       AND COUNTRY_CODE     = g_hts_ctry_code_us
       AND BEG_EFFECT_DATE <= p_import_date
       AND END_EFFECT_DATE >= p_import_date;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN 
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_OGA', p_entry_no, '232', 'SELECT', USER, 'CW',
               TO_CHAR(SQLCODE), 'INF', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'No effective tariff date range found'
              );

        RAISE SKIP_OGA_CHECKS;

      WHEN TOO_MANY_ROWS THEN
        p_load_status := SET_LOAD_STATUS('FALSE');
    
        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_OGA', p_entry_no, '233',
               'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'Overlapping Effective Tariff Date ranges exist'
              );

        RAISE SKIP_OGA_CHECKS;

      WHEN OTHERS THEN
        p_load_status := 'ERROR';
    
        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_OGA', p_entry_no, '234',
               'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'Error Selecting Tariff information.'
              );

        RAISE OGA_CHECKS_ERR;
  END;  


  MEDIA_CHECK( );

  FCC_CHECK( );

  FDA_CHECK( );

  DOT_CHECK( );


  EXCEPTION
    WHEN SKIP_OGA_CHECKS OR OGA_CHECKS_ERR THEN 
      NULL;

    WHEN OTHERS THEN 
      p_load_status := 'ERROR';
 
      p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_OGA', p_entry_no, '235', 'OGA CHECK',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               p_table || ', Item No ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)),
               'Unexpected SQL error in OGA stored procedure'
              );

END T_SP_RKP4290_OGA;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_OGA                 
** ************************************************************************* */
/
EXIT
