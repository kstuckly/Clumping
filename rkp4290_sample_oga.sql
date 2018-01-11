
/* *** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; *** */

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_SAMPLE_OGA

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_SAMPLE_OGA.SQL                                  **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_SAMPLE_OGA                                 **
**                                                                           **
**  AUTHOR   : KAVIN J STUCKLY                                               **
**  CREATED  : 12 DECEMBER 2001                                              **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedure T_SP_RKP4290_SAMPLE.  This procedure is to validate **
**             OGA (Other Government Agencies) information for a Non         **
**             Chargeable item with a single tariff number (no components).  **
**                                                                           **
** ************************************************************************* */
(
   p_entry_no             IN     ENTRY_HDR.ENTRY_NO%TYPE,
   p_owner                IN     ENTRY_INV.OWNER%TYPE,
   p_bill_of_lading_no    IN     ENTRY_INV.BILL_OF_LADING_NO%TYPE,
   p_import_date          IN     ENTRY_HDR.IMPORT_DATE%TYPE,
   p_item                 IN     ENTRY_DTL.ITEM%TYPE,
   p_movement_type        IN     SHIPPING_DTL.MOVEMENT_TYPE%TYPE,
   p_hts_no               IN     ENTRY_DTL.HTS_NO%TYPE,
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

   p3                     NUMBER;


   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */

   g_hts_ctry_code_us      CONSTANT HTS.COUNTRY_CODE%TYPE := 'US';

   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */



   SAMPLE_OGA_CHECKS_ERR  EXCEPTION;

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
            ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '700', 'IF', USER,
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
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_SAMPLE_OGA    **
** ************************************************************************* */

BEGIN
  IF (p_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
  BEGIN 
  p_load_status := 'TRUE';
 

           IF LTRIM(RTRIM(p_hts_no)) IS NULL THEN
               p_load_status := SET_LOAD_STATUS('WARN');

               p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '701', 'IF', USER, 'CW',
                         '1', 'INF', NULL,
                         'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                         'Non Chargeable Item ' || LTRIM(RTRIM(p_item)),
                         'Tariff number missing in HTS Ref No booster',
                         NULL
                        );

               RAISE SAMPLE_OGA_CHECKS_ERR;
           END IF;


/* ************************************************************************* */

    BEGIN
    SELECT NVL(UNIT_1, 'X'), NVL(UNIT_2, 'X'), NVL(UNIT_3, 'X'),
           NVL(OGA_1,  'X'), NVL(OGA_2,  'X'), NVL(OGA_3,  'X'),
           NVL(OGA_4,  'X'), NVL(OGA_5,  'X')

      INTO c_unit_1, c_unit_2, c_unit_3, c_oga_1, c_oga_2, c_oga_3,
           c_oga_4, c_oga_5

      FROM HTS

     WHERE HTS_NO           = p_hts_no
       AND COUNTRY_CODE     = g_hts_ctry_code_us
       AND BEG_EFFECT_DATE <= p_import_date
       AND END_EFFECT_DATE >= p_import_date;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN 
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '702', 'SELECT', USER, 'CW',
               TO_CHAR(SQLCODE), 'INF', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Non Chargeable Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'No effective tariff date range found'
              );

        RAISE SAMPLE_OGA_CHECKS_ERR;

      WHEN TOO_MANY_ROWS THEN
        p_load_status := SET_LOAD_STATUS('FALSE');
    
        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '703',
               'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Non Chargeable Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'Overlapping Effective Tariff Date ranges exist'
              );

        RAISE SAMPLE_OGA_CHECKS_ERR;

      WHEN OTHERS THEN
        p_load_status := 'ERROR';
    
        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '704',
               'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Non Chargeable Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)) || ' Import Date '
                      || RTRIM(p_import_date),
               'Error Selecting Tariff information.'
              );

        RAISE SAMPLE_OGA_CHECKS_ERR;
    END;  

/* ************************************************************************* */

  IF (c_unit_1 = 'LNM' OR c_unit_2 = 'LNM' or c_unit_3 = 'LNM') THEN
       p_load_status := SET_LOAD_STATUS('WARN');

       p3 := shrmsglog_pkg.snd_log_msg
             ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '705', 'IF',
              USER, 'CW', '1', 'INF', NULL,
              'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
              'Non Chargeable Item ' || p_item,
              '(LNM) Linear Meters is used',
              NULL
             );

  ELSIF (c_unit_1 = 'FBM' OR c_unit_2 = 'FBM' or c_unit_3 = 'FBM') THEN
          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '706', 'IF',
                 USER, 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Non Chargeable Item ' || p_item,
                 'FBM is used',
                 NULL
                );

  ELSIF (c_unit_1 = 'M2' OR c_unit_2 = 'M2' or c_unit_3 = 'M2') THEN
          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '707', 'IF',
                 USER, 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Non Chargeable Item ' || p_item,
                 '(M2) Squared Meters is used',
                 NULL
                );
  END IF;

/* ************************************************************************* */

  IF (c_oga_1 = 'FC4' OR c_oga_2 = 'FC4' OR c_oga_3 = 'FC4' OR
      c_oga_4 = 'FC4' OR c_oga_5 = 'FC4') THEN

       p_load_status := SET_LOAD_STATUS('WARN');

       p3 := shrmsglog_pkg.snd_log_msg
             ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '708', 'IF',
              USER, 'CW', '1', 'INF', NULL,
              'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
              'Non Chargeable Item '|| p_item,
              'HTS# ' || LTRIM(RTRIM(p_hts_no)),
              'FCC information is required for Customs clearance'
              );

  ELSIF (c_oga_1 = 'FC3' OR c_oga_2 = 'FC3' OR c_oga_3 = 'FC3' OR
         c_oga_4 = 'FC3' OR c_oga_5 = 'FC3') THEN

          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '709', 'IF',
                 USER, 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Non Chargeable Item '|| p_item,
                 'HTS# ' || LTRIM(RTRIM(p_hts_no)),
                 'FCC information may be required for Customs clearance'
                );
  END IF;  

/* ************************************************************************* */

  IF (c_oga_1 = 'FD2' OR c_oga_2 = 'FD2' OR c_oga_3 = 'FD2' OR
      c_oga_4 = 'FD2' OR c_oga_5 = 'FD2') THEN

       p_load_status := SET_LOAD_STATUS('WARN');
 
       p3 := shrmsglog_pkg.snd_log_msg
             ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '710', 'IF', USER,
              'CW', '1', 'INF', NULL,
              'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
              'Non Chargeable Item '|| p_item,
              'HTS# ' || LTRIM(RTRIM(p_hts_no)),
              'FDA Information is required for Customs clearance'
              );

  ELSIF (c_oga_1 = 'FD1' OR c_oga_2 = 'FD1' OR c_oga_3 = 'FD1' OR
         c_oga_4 = 'FD1' OR c_oga_5 = 'FD1') THEN

          p_load_status := SET_LOAD_STATUS('WARN');
 
          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '711', 'IF', USER,
                 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Item '|| p_item,
                 'HTS# ' || LTRIM(RTRIM(p_hts_no)),
                 'FDA information may be required for Customs clearance'
                );
  END IF;

/* ************************************************************************* */

  IF (c_oga_1 = 'FW2' OR c_oga_2 = 'FW2' OR c_oga_3 = 'FW2' OR
      c_oga_4 = 'FW2' OR c_oga_5 = 'FW2') THEN

       p_load_status := SET_LOAD_STATUS('WARN');
 
       p3 := shrmsglog_pkg.snd_log_msg
             ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '712', 'IF', USER,
              'CW', '1', 'INF', NULL,
              'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
              'Non Chargeable Item '|| p_item,
              'HTS# ' || LTRIM(RTRIM(p_hts_no)),
              'FWS Information is required for Customs clearance'
              );

  ELSIF (c_oga_1 = 'FW1' OR c_oga_2 = 'FW1' OR c_oga_3 = 'FW1' OR
         c_oga_4 = 'FW1' OR c_oga_5 = 'FW1') THEN

          p_load_status := SET_LOAD_STATUS('WARN');
 
          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '713', 'IF', USER,
                 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Item '|| p_item,
                 'HTS# ' || LTRIM(RTRIM(p_hts_no)),
                 'FWS information may be required for Customs clearance'
                );
  END IF;

/* ************************************************************************* */

  IF (c_oga_1 = 'DT2' OR c_oga_2 = 'DT2' OR c_oga_3 = 'DT2' OR
      c_oga_4 = 'DT2' OR c_oga_5 = 'DT2') THEN

       p_load_status := SET_LOAD_STATUS('WARN');
 
       p3 := shrmsglog_pkg.snd_log_msg
             ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '714', 'IF', USER,
              'CW', '1', 'INF', NULL,
              'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
              'Non Chargeable Item '|| p_item,
              'HTS# ' || LTRIM(RTRIM(p_hts_no)),
              'DOT Information is required for Customs clearance'
              );

  ELSIF (c_oga_1 = 'DT1' OR c_oga_2 = 'DT1' OR c_oga_3 = 'FD1' OR
         c_oga_4 = 'DT1' OR c_oga_5 = 'DT1') THEN

          p_load_status := SET_LOAD_STATUS('WARN');
 
          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '715', 'IF', USER,
                 'CW', '1', 'INF', NULL,
                 'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
                 'Non Chargeable Item '|| p_item,
                 'HTS# ' || LTRIM(RTRIM(p_hts_no)),
                 'DOT information may be required for Customs clearance'
                );
  END IF;
  END;  
  END IF;
/* ************************************************************************* */


  EXCEPTION
    WHEN SAMPLE_OGA_CHECKS_ERR THEN 
      NULL;

    WHEN OTHERS THEN 
      p_load_status := 'ERROR';
 
      p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE_OGA', p_entry_no, '716',
               'SAMPLE OGA CHECK', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
               SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Non Chargeable Item ' || LTRIM(RTRIM(p_item)),
               'HTS# ' || LTRIM(RTRIM(p_hts_no)),
               'Unexpected SQL error in Sample OGA stored procedure'
              );

END T_SP_RKP4290_SAMPLE_OGA;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_SAMPLE_OGA                 
** ************************************************************************* */
/
EXIT
