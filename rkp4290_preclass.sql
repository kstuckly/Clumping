
/* *** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; *** */

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_PRECLASS

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_PRECLASS.SQL                                    **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_PRECLASS                                   **
**                                                                           **
**  AUTHOR   : KAVIN J. STUCKLY                                              **
**  CREATED  : 14 DECEMBER 2000                                              **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedures T_SP_RKP4290 or T_SP_RKP4290_SAMPLE.  This         **
**             procedure will load the Entry Detail table using data from    **
**             the Pre-Class detail table.                                   **
**             This procedure will also call stored procedure                **
**             T_SP_RKP4290_OGA for OGA validation.                          **
**                                                                           **
** ************************************************************************* */
(
   p_table             IN     VARCHAR2,
   p_owner             IN     ENTRY_INV.OWNER%TYPE,
   p_equip_id          IN     SHIPPING_DTL.EQUIP_ID%TYPE,
   p_entry_no          IN     VARCHAR2,
   p_invoice_no_wk     IN     ENTRY_INV.INVOICE_NO%TYPE,
   p_invoice_no        IN     ENTRY_INV.INVOICE_NO%TYPE,
   p_item              IN     ENTRY_DTL.ITEM%TYPE,
   p_line_no           IN OUT ENTRY_DTL.LINE_NO%TYPE,
   p_item_desc         IN     ENTRY_DTL.ITEM_DESC%TYPE,
   p_ref_no            IN     ENTRY_DTL.REF_NO%TYPE,
   p_qty_entered       IN     ENTRY_DTL.QTY_ENTERED%TYPE,
   p_qty_um            IN     ENTRY_DTL.QTY_UM%TYPE,
   p_item_duty_amt     IN     ENTRY_DTL.ITEM_DUTY_AMT%TYPE,
   p_unit_duty         IN     ENTRY_DTL.UNIT_DUTY%TYPE,
   p_origin_country    IN     ENTRY_DTL.ORIGIN_COUNTRY%TYPE,
   p_rate_1            IN     ENTRY_DTL.RATE_1%TYPE,
   p_customs_val_1     IN     ENTRY_DTL.CUSTOMS_VAL_1%TYPE,
   p_rate_2            IN     ENTRY_DTL.RATE_2%TYPE,
   p_customs_val_2     IN     ENTRY_DTL.CUSTOMS_VAL_2%TYPE,
   p_rate_3            IN     ENTRY_DTL.RATE_3%TYPE,
   p_customs_val_3     IN     ENTRY_DTL.CUSTOMS_VAL_3%TYPE,
   p_rate_4            IN     ENTRY_DTL.RATE_4%TYPE,
   p_rate_5            IN     ENTRY_DTL.RATE_5%TYPE,
   p_rate_6            IN     ENTRY_DTL.RATE_6%TYPE,
   p_customs_val_6     IN     ENTRY_DTL.CUSTOMS_VAL_6%TYPE,
   p_rate_7            IN     ENTRY_DTL.RATE_7%TYPE,
   p_customs_val_7     IN     ENTRY_DTL.CUSTOMS_VAL_7%TYPE,
   p_edi_ind           IN OUT ENTRY_DTL.EDI_IND%TYPE,
   p_qty_avail_db      IN     ENTRY_DTL.QTY_AVAIL_DB%TYPE,
   p_import_date       IN     ENTRY_HDR.IMPORT_DATE%TYPE,
   p_item_gsp_status   IN     ITEM_DTL.STATUS%TYPE,
   p_price             IN     INVOICE_DTL.PRICE%TYPE,
   p_med_check         IN OUT VARCHAR2,
   p_bill_of_lading_no IN     ENTRY_INV.BILL_OF_LADING_NO%TYPE,
   p_foc_sample        IN     VARCHAR2,
   p_origin_ctry_sav   IN     ENTRY_DTL.ORIGIN_COUNTRY%TYPE,
   p_load_status          OUT VARCHAR2
   )

AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   c_hts_no               ENTRY_DTL.HTS_NO%TYPE;
   c_item_desc            ENTRY_DTL.ITEM_DESC%TYPE;
   c_item_duty_amt        ENTRY_DTL.ITEM_DUTY_AMT%TYPE;
   c_unit_duty            ENTRY_DTL.UNIT_DUTY%TYPE;
   c_origin_country       ENTRY_DTL.ORIGIN_COUNTRY%TYPE;
   c_customs_val_1        ENTRY_DTL.CUSTOMS_VAL_1%TYPE;
   c_rate_2               ENTRY_DTL.RATE_2%TYPE;
   c_customs_val_2        ENTRY_DTL.CUSTOMS_VAL_2%TYPE;
   c_rate_4               ENTRY_DTL.RATE_4%TYPE;
   c_rate_5               ENTRY_DTL.RATE_5%TYPE;
   c_component_desc       PRECLASS_DTL.ITEM_DESC%TYPE;
   c_export_country       PRECLASS_DTL.EXPORT_COUNTRY%TYPE;
   c_item_first_cost      PRECLASS_DTL.CUSTOMS_VAL%TYPE;
   c_hts_line_no          PRECLASS_DTL.LINE_NO%TYPE;
   c_spec_prog1           HTS.SPEC_PROG1%TYPE;
   c_spec_prog2           HTS.SPEC_PROG2%TYPE;
   c_spec_prog3           HTS.SPEC_PROG3%TYPE;
   c_spec_prog4           HTS.SPEC_PROG4%TYPE;
   c_spec_prog5           HTS.SPEC_PROG5%TYPE;
   c_spec_prog6           HTS.SPEC_PROG6%TYPE;
   c_spec_prog7           HTS.SPEC_PROG7%TYPE;
   c_spec_prog8           HTS.SPEC_PROG8%TYPE;
   c_spec_prog9           HTS.SPEC_PROG9%TYPE;
   c_spec_prog10          HTS.SPEC_PROG10%TYPE;
   c_hts_spec_prog        COUNTRY.HTS_SPEC_PROG%TYPE;

   p3                     NUMBER;
   c_load_status          VARCHAR2(10);



   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */


   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */



   PRECLASS_ERROR         EXCEPTION;

/* ************************************************************************* **
**                 CURSORS                                                   **
** ************************************************************************* */

   CURSOR C_PRECLASS_DTL IS
   SELECT HTS_NO, LINE_NO, NVL(ITEM_DESC, c_item_desc), CUSTOMS_VAL,
          EXPORT_COUNTRY

     FROM PRECLASS_DTL

    WHERE OWNER       = p_owner
      AND PRECLASS_NO = p_item
      AND ITEM        = p_item
    ORDER BY LINE_NO;

/* ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL FUNCTION SET_LOAD_STATUS              
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
            ('T_SP_RKP4290_PRECLASS', p_entry_no, '101', 'IF', USER,
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
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_PRECLASS      **
** ************************************************************************* */

BEGIN
  c_item_desc      :=  p_item_desc;
  c_item_duty_amt  :=  p_item_duty_amt;
  c_unit_duty      :=  p_unit_duty;
  c_origin_country :=  p_origin_country;
  c_customs_val_1  :=  p_customs_val_1;
  c_rate_2         :=  p_rate_2;
  c_customs_val_2  :=  p_customs_val_2;
  c_rate_4         :=  p_rate_4;
  c_rate_5         :=  p_rate_5;

  p_load_status    :=  'TRUE';
  c_load_status    :=  'TRUE';


  BEGIN
    SELECT NVL(CUSTOMS_VAL, 0) INTO c_item_first_cost
      FROM PRECLASS_DTL
     WHERE OWNER       = p_owner
       AND PRECLASS_NO = p_item
       AND ITEM        = p_item
       AND LINE_NO     = 1;

    IF p_table = 'ITEM' and p_price != c_item_first_cost THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '102', 'COMPARE', USER,
               'CW', '1', 'INF', NULL,
               'Item '     || LTRIM(RTRIM(p_item)),
               'Invoice# ' || LTRIM(RTRIM(p_invoice_no)),
               'Pre-Class total value is different from Invoice price.',
               'Update Pre-Class Table to reflect new price.'
               );
      
    ELSIF p_table = 'SAMPLE' and p_price != c_item_first_cost THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '103', 'COMPARE', USER,
               'CW', '1', 'INF', NULL,
               'B/L ' || p_bill_of_lading_no,
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'Item ' || LTRIM(RTRIM(p_item)),
               'Sample Pre-Class total value is different from Shipment price'
               );
    END IF;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        IF p_table = 'ITEM' THEN
            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290_PRECLASS', p_entry_no, '104', 'SELECT',
                   USER, 'CW', TO_CHAR(SQLCODE), 'INF', SQLERRM,
                   'Item '     || LTRIM(RTRIM(p_item)),
                   'Invoice# ' || LTRIM(RTRIM(p_invoice_no)),
                   'Pre-Class total value is missing.',
                   'Update Pre-Class Table to reflect total value.'
                   );
        ELSE
            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290_PRECLASS', p_entry_no, '105', 'SELECT',
                   USER, 'CW', TO_CHAR(SQLCODE), 'INF', SQLERRM,
                   'B/L ' || p_bill_of_lading_no,
                   'Container ' || LTRIM(RTRIM(p_equip_id)),
                   'Item ' || LTRIM(RTRIM(p_item)),
                   'Sample Pre-Class total value is missing.'
                  );
        END IF;

      WHEN OTHERS THEN 
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '106', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || LTRIM(RTRIM(p_item)),
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'An SQL error while selecting Pre-Class total value.'
               );

        RAISE PRECLASS_ERROR;
  END; 

  BEGIN
    OPEN C_PRECLASS_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '107', 'OPEN', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || LTRIM(RTRIM(p_item)),
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'An SQL error while Opening Pre-Class Detail cursor'
               );
        RAISE PRECLASS_ERROR;

  END;

  c_hts_line_no := 0;

  <<L_4>>
  LOOP
    c_hts_no          := NULL;
    c_hts_line_no     := NULL;
    c_component_desc  := NULL;
    c_item_first_cost := 0;
    c_export_country  := NULL;

    BEGIN
      FETCH C_PRECLASS_DTL
       INTO c_hts_no, c_hts_line_no, c_component_desc,
            c_item_first_cost, c_export_country; 

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_PRECLASS', p_entry_no, '108', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Item '  || LTRIM(RTRIM(p_item)),
                 'Error retrieving data from Pre-Class detail cursor',
                 NULL, NULL
                 );
 
          RAISE PRECLASS_ERROR;
    END;

    EXIT L_4 WHEN C_PRECLASS_DTL%NOTFOUND;

/* ************************************************************************* */
 
    IF c_hts_line_no > TRUNC(c_hts_line_no) THEN
        c_customs_val_1 := c_item_first_cost;
 
        T_SP_RKP4290_OGA('PRECLASS', p_owner, p_entry_no, p_bill_of_lading_no,
                         p_import_date, c_export_country, p_item,
                         c_hts_no, c_hts_line_no, p_foc_sample,
                         p_origin_ctry_sav, p_med_check, c_load_status);
 
        p_load_status := SET_LOAD_STATUS(c_load_status);

	IF p_load_status = 'ERROR' THEN
            RAISE PRECLASS_ERROR;
        END IF;

        p_line_no        := TRUNC(p_line_no) +
                            (c_hts_line_no - TRUNC(c_hts_line_no));
 
        c_item_desc      := c_component_desc;
        c_rate_2         := 0;
        c_customs_val_2  := 0;
        c_rate_4         := 0;
        c_rate_5         := 0;
        c_origin_country := c_export_country;
        c_item_duty_amt  := 0;
        c_unit_duty      := 0;

    END IF;   /* IF c_hts_line_no > TRUNC(c_hts_line_no) */
 
/* ************************************************************************* */
 
    BEGIN

      /* *******************************************************************
       * Kavin 2/21/2013 Item Duty Amt will now be stored in both 
       * ITEM_DUTY_AMT and HTS_DUTY_AMT.  The Item Duty Amount is the
       * Quoted Duty or Estimated Duty at the time the entry is created.
       * After entry is cleared by U.S. Customs, program Rkp4296 will
       * update the ITEM_DUTY_AMT with the "real" duty amt. Since
       * HTS_DUTY_AMT contains Quoted Duty, Rockport Impromptu will be able to
       * report difference between estimated duty and real duty at item 
       * level of the entry.
       * ******************************************************************* */

      INSERT INTO ENTRY_DTL
          (OWNER, ENTRY_NO, INVOICE_NO, ITEM, LINE_NO, ITEM_DESC,
           REF_NO, QTY_ENTERED, QTY_UM, ITEM_DUTY_AMT, UNIT_DUTY,
           ORIGIN_COUNTRY, HTS_NO, UNIT_EXT_IND, RATE_1,
           CUSTOMS_VAL_1, RATE_2, CUSTOMS_VAL_2, RATE_3,
           CUSTOMS_VAL_3, RATE_4, CUSTOMS_VAL_4, RATE_5,
           CUSTOMS_VAL_5, RATE_6, CUSTOMS_VAL_6, RATE_7,
           CUSTOMS_VAL_7, EDI_IND, QTY_AVAIL_DB, LAST_USER,
           LAST_UPDATE, LAST_ACTIVITY, P_LINK_KEY_VAL,
           HTS_DUTY_AMT
          )
  
          VALUES(p_owner, p_entry_no, p_invoice_no_wk, p_item, p_line_no,
                 c_item_desc, p_ref_no, p_qty_entered, p_qty_um,
                 p_item_duty_amt, p_unit_duty, c_origin_country, c_hts_no,
                 'E', p_rate_1, c_customs_val_1, c_rate_2, c_customs_val_2,
                 p_rate_3, p_customs_val_3, c_rate_4, c_customs_val_1,
                 c_rate_5, c_customs_val_1, p_rate_6, p_customs_val_6,
                 p_rate_7, p_customs_val_7, p_edi_ind,
                 p_qty_avail_db, 'RKP4290', SYSDATE, 'A',
                 p_entry_no || ' + ' || p_invoice_no_wk || ' + ' || p_item || ' + ' ||
                     TO_CHAR(p_line_no, 'FM99999.09'),
                 p_item_duty_amt
                );
 
      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
               ('T_SP_RKP4290_PRECLASS', p_entry_no, '112', 'INSERT', USER,
                'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                'B/L ' || p_bill_of_lading_no,
                'Container ' || LTRIM(RTRIM(p_equip_id)),
                'Item ' || LTRIM(RTRIM(p_item)) || ' HTS# ' ||
                      c_hts_no,
                'Error inserting component into Entry Dtl, '
                      || 'Entry Dtl Line No ' || p_line_no
               );

          RAISE PRECLASS_ERROR;
    END; 

    IF p_table = 'ITEM' and p_edi_ind = 'A' THEN
        p_load_status := SET_LOAD_STATUS('WARN');
 
        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '113', 'IF', USER,
               'CW', '1', 'INF', NULL,
               'B/L '|| p_bill_of_lading_no, 
               'Invoice '|| p_invoice_no,
               'Item ' || p_item,
               'GSP has expired.'
               );
 
        p_edi_ind := NULL;
    END IF;
 
  END LOOP L_4;
 
  BEGIN
    CLOSE C_PRECLASS_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '114', 'CLOSE', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || LTRIM(RTRIM(p_item)),
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'Error closing Pre-Class Detail cursor'
               );

        RAISE PRECLASS_ERROR;
  END;
  
  p_line_no := TRUNC(p_line_no);

  EXCEPTION
    WHEN PRECLASS_ERROR THEN
      p_line_no := TRUNC(p_line_no);

      IF C_PRECLASS_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_PRECLASS_DTL;
    
            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

    WHEN OTHERS THEN 
      p_line_no     := TRUNC(p_line_no);
      p_load_status := 'ERROR';
 
      p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_PRECLASS', p_entry_no, '115', 'EXCEPTION',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Item No ' || LTRIM(RTRIM(p_item)),
               'Entry Detail Line No ' || p_line_no,
               'Unexpected SQL error in PRECLASS stored procedure'
              );

      IF C_PRECLASS_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_PRECLASS_DTL;
    
            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

END T_SP_RKP4290_PRECLASS;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_PRECLASS
** ************************************************************************* */
/
EXIT
