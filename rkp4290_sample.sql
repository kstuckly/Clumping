
CREATE OR REPLACE PROCEDURE T_SP_RKP4290_SAMPLE

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_SAMPLE.SQL                                      **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_SAMPLE                                     **
**                                                                           **
**  AUTHOR   : KAVIN J STUCKLY                                               **
**  CREATED  : 14 DECEMBER 2000                                              **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedure T_SP_RKP4290.  This procedure is to process         **
**             Non-Chargeable Samples and load the Entry tables.             **
**                                                                           **
** ************************************************************************* */
(
   p_entry_no          IN     ENTRY_HDR.ENTRY_NO%TYPE,
   p_owner             IN     ENTRY_INV.OWNER%TYPE,
   p_bill_of_lading_no IN     ENTRY_INV.BILL_OF_LADING_NO%TYPE,
   p_import_date       IN     ENTRY_HDR.IMPORT_DATE%TYPE,
   p_entry_port        IN     ENTRY_HDR.ENTRY_PORT%TYPE,
   p_invoice_no        IN     ENTRY_INV.INVOICE_NO%TYPE,
   p_item              IN     ENTRY_DTL.ITEM%TYPE,
   p_item_gsp_status   IN     ITEM_DTL.STATUS%TYPE,
   p_origin_country    IN     ENTRY_DTL.ORIGIN_COUNTRY%TYPE,
   p_manufacturer      IN     ENTRY_INV.MANUFACTURER%TYPE,
   p_edi_ind           IN OUT ENTRY_DTL.EDI_IND%TYPE,
   p_line_no           IN OUT ENTRY_DTL.LINE_NO%TYPE,
   p_med_check         IN OUT VARCHAR2, 
   p_load_status          OUT VARCHAR2
)

AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   l_equip_id             SHIPPING_DTL.EQUIP_ID%TYPE;
   l_ref_no               SHIPPING_DTL.REF_NO%TYPE;
   l_shp_line_no          SHIPPING_DTL.LINE_NO%TYPE;
   l_link_tbl_name        REFITEM.LINK_TBL_NAME%TYPE;
   l_link_key_val         REFITEM.LINK_KEY_VAL%TYPE;
   l_invoice_no           ENTRY_INV.INVOICE_NO%TYPE;

   c_p_link_key_val       ENTRY_INV.P_LINK_KEY_VAL%TYPE;

   c_ref_no               SHIPPING_DTL.REF_NO%TYPE;
   c_ship_contract_no     SHIPPING_DTL.CONTRACT_NO%TYPE;
   c_ship_edi_ind         SHIPPING_DTL.EDI_IND%TYPE;
   c_movement_type        SHIPPING_DTL.MOVEMENT_TYPE%TYPE;   
   c_price                INVOICE_DTL.PRICE%TYPE;
   c_buyer                INVOICE_HDR.BUYER%TYPE;
   c_invoice_no_wk        ENTRY_INV.INVOICE_NO%TYPE;
   c_hmf_ind              PLCI3225.HMF_IND%TYPE;

   c_item                 ENTRY_DTL.ITEM%TYPE;
   c_last_item            ENTRY_DTL.ITEM%TYPE;
   c_item_desc            ENTRY_DTL.ITEM_DESC%TYPE;
   c_qty_entered          ENTRY_DTL.QTY_ENTERED%TYPE;
   c_qty_um               ENTRY_DTL.QTY_UM%TYPE;
   c_item_duty_amt        ENTRY_DTL.ITEM_DUTY_AMT%TYPE;
   c_unit_duty            ENTRY_DTL.UNIT_DUTY%TYPE;
   c_hts_no               ENTRY_DTL.HTS_NO%TYPE;
   c_rate_1               ENTRY_DTL.RATE_1%TYPE;
   c_customs_val_1        ENTRY_DTL.CUSTOMS_VAL_1%TYPE;
   c_rate_2               ENTRY_DTL.RATE_2%TYPE;
   c_customs_val_2        ENTRY_DTL.CUSTOMS_VAL_2%TYPE;
   c_rate_3               ENTRY_DTL.RATE_3%TYPE;
   c_customs_val_3        ENTRY_DTL.CUSTOMS_VAL_3%TYPE;
   c_rate_4               ENTRY_DTL.RATE_4%TYPE;
   c_customs_val_4        ENTRY_DTL.CUSTOMS_VAL_4%TYPE;
   c_rate_5               ENTRY_DTL.RATE_5%TYPE;
   c_customs_val_5        ENTRY_DTL.CUSTOMS_VAL_5%TYPE;
   c_rate_6               ENTRY_DTL.RATE_6%TYPE;
   c_customs_val_6        ENTRY_DTL.CUSTOMS_VAL_6%TYPE;
   c_rate_7               ENTRY_DTL.RATE_7%TYPE;
   c_customs_val_7        ENTRY_DTL.CUSTOMS_VAL_7%TYPE;
   c_qty_avail_db         ENTRY_DTL.QTY_AVAIL_DB%TYPE;

   p3                     NUMBER;
   l_sql_ret_code         NUMBER;
   c_load_status          VARCHAR2(10);
   c_orig_first_cost      NUMBER;


   /* ************************************************************************** **
   **                      Constants for Performance Tunning                     **
   ** ************************************************************************** */

   g_edi_ind_y            CONSTANT SHIPPING_DTL.EDI_IND%TYPE     := 'Y'; 
   g_reference_qlf_dsc    CONSTANT REFITEM.REFERENCE_QLF%TYPE    := 'DSC'; 
   g_reference_qlf_hts    CONSTANT REFITEM.REFERENCE_QLF%TYPE    := 'HTS'; 
   g_reference_qlf_vfc    CONSTANT REFITEM.REFERENCE_QLF%TYPE    := 'VFC'; 
   g_exp_category_tax     CONSTANT EXPENSE.EXPENSE_CATEGORY%TYPE := 'TAX'; 
   g_exp_code_hmf         CONSTANT EXPENSE.EXPENSE_CODE%TYPE     := 'HMF'; 
   g_matrix_id_ctn        CONSTANT MATRIX_DTL.MATRIX_ID%TYPE     := 'CTN'; 

   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */


   SAMPLE_ERROR           EXCEPTION;


/* ************************************************************************* **
**                 CURSORS                                                   **
** ************************************************************************* */

   CURSOR C_SAMPLE IS
   SELECT ITEM, CONTRACT_NO, INVOICE_NO, EQUIP_ID, REF_NO,
          NVL(QTY, 0), QTY_UM, MOVEMENT_TYPE

     FROM SHIPPING_DTL

    WHERE OWNER             = LTRIM(RTRIM(p_owner))
      AND BILL_OF_LADING_NO = LTRIM(RTRIM(p_bill_of_lading_no))
      AND ITEM              = LTRIM(RTRIM(p_item))
      AND INVOICE_NO        = LTRIM(RTRIM(p_invoice_no))
      AND EDI_IND           = g_edi_ind_y;

/* ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL FUNCTION SET_LOAD_STATUS              

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
            ('T_SP_RKP4290_SAMPLE', p_entry_no, '300', 'IF', USER,
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
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_SAMPLE        **
** ************************************************************************* */

BEGIN
  p_load_status     := 'TRUE';
  c_load_status     := 'TRUE';
  c_rate_1          := 0;
  c_rate_2          := 0;
  c_rate_3          := 0;
  c_rate_4          := 0;
  c_rate_5          := 0;
  c_rate_6          := 0;
  c_rate_7          := 0;
  c_customs_val_1   := 0;
  c_customs_val_2   := 0;
  c_customs_val_3   := 0;
  c_customs_val_4   := 0;
  c_customs_val_5   := 0;
  c_customs_val_6   := 0;
  c_customs_val_7   := 0;

  c_orig_first_cost := 0;

  c_p_link_key_val  := NULL;

  c_p_link_key_val  := p_entry_no || ' + ' || p_invoice_no;


  BEGIN

    INSERT INTO ENTRY_INV
              (OWNER, ENTRY_NO, INVOICE_NO, manufacturer, INV_TOTAL_VAL,
               REFERENCE_CURRENCY, ROE, INVOICE_DESC,
               ADJUST_VALUE, BILL_OF_LADING_NO,
               LAST_USER, LAST_UPDATE, LAST_ACTIVITY,
               P_LINK_KEY_VAL
              )
       VALUES(p_owner, p_entry_no, p_invoice_no, p_manufacturer,0, 'US', '1',
              'Electrical Equipment', 'N', p_bill_of_lading_no,
              'RKP4290', SYSDATE, 'A',
              c_p_link_key_val
             );

    EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN 
        p3 := 0;

      WHEN OTHERS THEN 
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE', p_entry_no, '301', 'INSERT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Invoice No ' || LTRIM(RTRIM(p_invoice_no)),
               'Error creating Entry Invoice for Non-Chargeable Sample',
               NULL
              );

        RAISE SAMPLE_ERROR;
  END;

  BEGIN
    OPEN C_SAMPLE;

    EXCEPTION
      WHEN OTHERS THEN 
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE', p_entry_no, '302', 'OPEN', USER, 
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L '|| p_bill_of_lading_no,
               'Item ' || p_item,
               'Error opening Non-Chargeable Sample cursor',
               NULL
               );

        RAISE SAMPLE_ERROR;
  END;

  <<L_0>>
  LOOP
    c_qty_entered   := 0;
    c_customs_val_1 := 0;
    c_qty_um        := NULL;
    l_equip_id      := NULL;
    l_invoice_no    := NULL;

    BEGIN
      FETCH C_SAMPLE
       INTO c_item, c_ship_contract_no, l_invoice_no, l_equip_id, l_ref_no,
            c_qty_entered, c_qty_um, c_movement_type;

      EXCEPTION
        WHEN OTHERS THEN 
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_SAMPLE', p_entry_no, '303', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| p_bill_of_lading_no,
                 'Item ' || p_item,
                 'Error retrieving data from Non-Chargeable Sample cursor',
                 NULL
                 );

          RAISE SAMPLE_ERROR;
    END; 

    EXIT L_0 WHEN C_SAMPLE%NOTFOUND;

    c_ref_no := c_ship_contract_no;

/* ************************************************************************* */

    BEGIN
      c_item_desc := NULL;

      IF (c_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
         BEGIN
         l_link_tbl_name := 'SHIPPING - ' ||
                              LTRIM(RTRIM(p_bill_of_lading_no));

         l_link_key_val := LTRIM(RTRIM(p_bill_of_lading_no)) ||' + '||
                             LTRIM(RTRIM(l_equip_id)) ||' + '||
                             LTRIM(RTRIM(c_item)) ||' + '||
                             LTRIM(RTRIM(l_ref_no));


         SELECT UNIQUE REF_NO INTO c_item_desc

           FROM REFITEM

          WHERE OWNER         = p_owner
            AND LINK_TBL_NAME = l_link_tbl_name
            AND LINK_KEY_VAL  = l_link_key_val
            AND REFERENCE_QLF = g_reference_qlf_dsc;


         EXCEPTION
           WHEN NO_DATA_FOUND THEN 
             p_load_status := SET_LOAD_STATUS('WARN');

             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '304', 'SELECT',
                    USER, 'CW', '1', 'INF', NULL,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Non-chargeable sample item description (DSC Ref_no '
                         || 'booster) is missing.'
                   );

           WHEN TOO_MANY_ROWS THEN 
             p_load_status := SET_LOAD_STATUS('FALSE');

             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '305', 'SELECT', USER,
                    'CF', '1', 'INF', NULL,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Multiple Non-chargeable sample item descriptions (DSC Ref No '
                         || 'booster) exist.'
                   );

           WHEN OTHERS THEN 
             c_item_desc   := NULL;
             p_load_status := 'ERROR';
   
             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '306', 'SELECT', USER,
                    'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Error retrieving non-chargeable sample item description '
                        || '(DSC Ref_No booster).'
                   );

             RAISE SAMPLE_ERROR;
           END;
        END IF;              
    END;

/* ************************************************************************* */

    BEGIN
      c_hts_no := NULL;

      IF (c_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
         BEGIN
    
         SELECT UNIQUE REF_NO INTO c_hts_no

           FROM REFITEM

             WHERE OWNER         = p_owner
            AND LINK_TBL_NAME = l_link_tbl_name
            AND LINK_KEY_VAL  = l_link_key_val
            AND REFERENCE_QLF = g_reference_qlf_hts;
   
         EXCEPTION
           WHEN NO_DATA_FOUND THEN 
             p_load_status := SET_LOAD_STATUS('WARN');
   
             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '307', 'SELECT', USER, 'CW',
                    '1', 'INF', NULL,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Non-chargeable sample HTS# (HTS Ref No booster) '
                        ||'is missing.'
                   );
   
           WHEN TOO_MANY_ROWS THEN
             c_item_desc   := NULL;
             c_hts_no      := NULL;
             p_load_status := SET_LOAD_STATUS('FALSE');
   
             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '308', 'SELECT', USER, 'CF',
                    TO_CHAR(SQLCODE), 'INF', SQLERRM,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Multiple non-chargeable sample HTS# (HTS Ref No booster)'
                        || ' exist.'
                    );
    
           WHEN OTHERS THEN 
             c_item_desc   := NULL;
             p_load_status := 'ERROR';
   
             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290_SAMPLE', p_entry_no, '309', 'SELECT', USER, 'CE',
                    TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                    'B/L '|| p_bill_of_lading_no,
                    'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Error selecting HTS Ref No booster for non-chargeable'
                        || ' sample'
                    );
           END;
        END IF;              
    END;


/* ************************************************************************* */

    BEGIN
      c_customs_val_1 := 0;
      c_price         := 0;

      IF (c_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
         BEGIN
         SELECT UNIQUE TO_NUMBER(REF_NO) INTO c_customs_val_1
   
           FROM REFITEM
   
          WHERE OWNER         = p_owner
            AND LINK_TBL_NAME = l_link_tbl_name
            AND LINK_KEY_VAL  = l_link_key_val
            AND REFERENCE_QLF = g_reference_qlf_vfc;
   
         c_price := c_customs_val_1;
   
         c_customs_val_1 := c_customs_val_1 * c_qty_entered;
   
   
         EXCEPTION
            WHEN NO_DATA_FOUND THEN 
               p_load_status := SET_LOAD_STATUS('FALSE');
   
               p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290_SAMPLE', p_entry_no, '310', 'SELECT', USER, 'CF',
                     TO_CHAR(SQLCODE), 'INF', SQLERRM,
                     'B/L '|| p_bill_of_lading_no,
                     'Item ' || c_item,
                     'Container ' || l_equip_id || ' Line ' || l_ref_no,
                      'Non-chargeable sample value (VFC Ref_No booster) is '
                         || 'missing'
                     );
   
         	WHEN TOO_MANY_ROWS THEN
                p_load_status := SET_LOAD_STATUS('FALSE');
      
                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290_SAMPLE', p_entry_no, '311', 'SELECT', USER, 'CF',
                       TO_CHAR(SQLCODE), 'INF', SQLERRM,
                       'B/L '|| p_bill_of_lading_no,
                       'Item ' || c_item,
                    'Container ' || l_equip_id || ' Line ' || l_ref_no,
                    'Multiple Non-chargeable sample values (VFC Ref No booster) '
                        || 'exist'
                    );
   
            WHEN OTHERS THEN 
               c_item_desc   := NULL;
               p_load_status := 'ERROR';
   
               p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290_SAMPLE', p_entry_no, '312', 'SELECT', USER, 'CE',
                     TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'B/L '|| p_bill_of_lading_no,
                     'Item ' || c_item,
                     'Container ' || l_equip_id || ' Line ' || l_ref_no,
                     'Error selecting Non-chargeable sample value '
                          || '(VFC Ref No booster)'
                     );
   
               RAISE SAMPLE_ERROR;
         END; 
      END IF;              
   
    END;

/* ************************************************************************* */

    BEGIN
      c_hmf_ind := NULL;
      c_rate_5 := 0;

      IF (c_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
         BEGIN

         SELECT HMF_IND INTO c_hmf_ind
           FROM PLCI3225
          WHERE PLACE_CODE = p_entry_port;
   
         IF c_hmf_ind = 'Y' THEN
             BEGIN
               SELECT NVL(RATE_PER_UNIT_1 * c_customs_val_1, 0)
                 INTO c_rate_5
                 FROM EXPENSE
                WHERE EXPENSE_CATEGORY = g_exp_category_tax
                  AND EXPENSE_CODE     = g_exp_code_hmf;
    
               EXCEPTION
                 WHEN NO_DATA_FOUND THEN 
                   c_rate_5 := 0;
   
                 WHEN OTHERS THEN
                   p_load_status := 'ERROR';
   
                   p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290_SAMPLE', p_entry_no, '313', 'SELECT', USER,
                         'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM, 
                         'Expense Category TAX',
                         'Expense Code HMF',
                         'Error selecting Harbor Maintenance Fee rate '
                            || 'from EXPENSE',
                         NULL
                        );
   
                  RAISE SAMPLE_ERROR;
             END;
   
         END IF;

    
         EXCEPTION
           WHEN NO_DATA_FOUND THEN 
             c_rate_5 := 0;
   
           WHEN OTHERS THEN
             p_load_status := 'ERROR';
   
             p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290_SAMPLE', p_entry_no, '314', 'SELECT', USER, 'CE',
                   TO_CHAR(SQLCODE), 'ERR', SQLERRM, 
                   'Place Code ' || p_entry_port, 
                   'Error selecting Harbor Maintenance Fee Ind from PLCI3225',
                   NULL, NULL
                  );
   
             RAISE SAMPLE_ERROR;
         END; 
      END IF;              
    END;


/* ************************************************************************* */

    c_customs_val_6 := 0;
    c_rate_6        := 0;
    c_customs_val_7 := 0;
    c_rate_7        := 0;
    c_item_duty_amt := 0;
    c_unit_duty     := 0;

/* ************************************************************************* */

    BEGIN
      c_qty_avail_db := 0;

      IF (c_movement_type NOT IN ('COU', 'HAND', 'POST')) THEN
         BEGIN
         SELECT NVL(SUM(NVL(PACK_TOTAL, 0)),0) INTO c_qty_avail_db
           FROM MATRIX_DTL
          WHERE OWNER         = p_owner
            AND LINK_TBL_NAME ='SHIPPING - '||LTRIM(RTRIM(p_bill_of_lading_no))
            AND LINK_KEY_VAL || TRUNC(ROW_NO)  IN 
                  (SELECT LTRIM(RTRIM(p_bill_of_lading_no)) || ' + ' ||
                          LTRIM(RTRIM(EQUIP_ID)) || REF_NO
                     FROM SHIPPING_DTL
                    WHERE OWNER             = p_owner
                      AND BILL_OF_LADING_NO = LTRIM(RTRIM(p_bill_of_lading_no))
                      AND EQUIP_ID          = LTRIM(RTRIM(l_equip_id))
                      AND ITEM              = LTRIM(RTRIM(c_item))
                      AND REF_NO            = l_ref_no
                      AND EDI_IND           = g_edi_ind_y
                  )
   
            AND MATRIX_ID = g_matrix_id_ctn
            AND (LTRIM(RTRIM(ROW_NAME)) IS NULL OR RTRIM(ROW_NAME) =  g_matrix_id_ctn)
            AND ROW_CODE  = LTRIM(RTRIM(c_item));
   
         EXCEPTION
           WHEN NO_DATA_FOUND THEN
             c_qty_avail_db := 0;
   
           WHEN OTHERS THEN
             p_load_status := 'ERROR';
   
             p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290_SAMPLE', p_entry_no, '315', 'SELECT', USER,
                   'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM, 
                   'B/L ' || p_bill_of_lading_no,
                   'Container ' || l_equip_id,
                   'Non-Chargeable Sample Item ' || c_item,
                   'Error selecting Total Cartons from MATRIX_DTL'
                  );
   
             RAISE SAMPLE_ERROR;
         END; 
      END IF;              
    END;

/* ************************************************************************* */

    IF c_hts_no = 'MULTI' THEN
        c_invoice_no_wk := p_invoice_no;

        T_SP_RKP4290_PRECLASS('SAMPLE', p_owner, l_equip_id, p_entry_no,
                              c_invoice_no_wk, p_invoice_no, c_item,
                              p_line_no, c_item_desc, c_ref_no,
                              c_qty_entered, c_qty_um, c_item_duty_amt,
                              c_unit_duty, p_origin_country, c_rate_1,
                              c_customs_val_1, c_rate_2, c_customs_val_2,
                              c_rate_3, c_customs_val_3, c_rate_4,
                              c_rate_5, c_rate_6, c_customs_val_6,
                              c_rate_7, c_customs_val_7 * c_qty_entered,
                              p_edi_ind, c_qty_avail_db,
                              p_import_date, p_item_gsp_status,
                              c_price, p_med_check, p_bill_of_lading_no,
                              'TRUE', p_origin_country, c_load_status
                             );

	p_load_status := SET_LOAD_STATUS(c_load_status);

        IF p_load_status = 'ERROR' THEN
            RAISE SAMPLE_ERROR;
        END IF;

    ELSE    
        T_SP_RKP4290_SAMPLE_OGA(p_entry_no, p_owner, p_bill_of_lading_no,
                                p_import_date, c_item, c_movement_type, c_hts_no,
                                c_load_status
                               );

        p_load_status := SET_LOAD_STATUS(c_load_status);

        IF p_load_status = 'ERROR' THEN
            RAISE SAMPLE_ERROR;
        END IF;

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
                 ORIGIN_COUNTRY, HTS_NO, UNIT_EXT_IND, RATE_1, CUSTOMS_VAL_1,
                 RATE_2, CUSTOMS_VAL_2, RATE_3, CUSTOMS_VAL_3, RATE_4,
                 CUSTOMS_VAL_4, RATE_5, CUSTOMS_VAL_5, RATE_6, CUSTOMS_VAL_6,
                 RATE_7, CUSTOMS_VAL_7, EDI_IND, DIRECT_ID,QTY_AVAIL_DB, LAST_USER,
                 LAST_UPDATE, LAST_ACTIVITY, P_LINK_KEY_VAL, HTS_DUTY_AMT)

             VALUES(p_owner, p_entry_no, p_invoice_no, p_item, p_line_no,
                  c_item_desc, c_ref_no, c_qty_entered, c_qty_um,
                  c_item_duty_amt, c_unit_duty, p_origin_country, c_hts_no,
                  'E', c_rate_1, c_customs_val_1, c_rate_2, c_customs_val_2,
                  c_rate_3, c_customs_val_3, c_rate_4, c_customs_val_1,
                  c_rate_5, c_customs_val_1, c_rate_6, c_customs_val_6,
                  c_rate_7, c_customs_val_7 * c_qty_entered,
                  p_edi_ind, 'Y',c_qty_avail_db, 'RKP4290', SYSDATE, 'A',
                  p_entry_no || ' + ' || p_invoice_no || ' + ' || p_item || ' + ' ||
                      TO_CHAR(p_line_no, 'FM99999.09'),
                  c_item_duty_amt
                 );

          EXCEPTION
            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290_SAMPLE', p_entry_no, '316', 'INSERT', USER,
                     'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'B/L ' || p_bill_of_lading_no,
                     'Container ' || LTRIM(RTRIM(l_equip_id)),
                     'Item ' || LTRIM(RTRIM(p_item)) || ' HTS# ' ||
                           c_hts_no,
                     'Error adding Non-Chargeable Sample to Entry Dtl'
                           || 'Entry Dtl Line No ' || p_line_no
                    );

             RAISE SAMPLE_ERROR;
        END; 

    END IF;

    T_SP_RKP4290_REFITEM( p_entry_no,
                          p_owner,
                          'NCS',
                          p_invoice_no,
                          l_ref_no,
                          p_item,     
                          p_line_no,
                          l_equip_id,
                          NULL,
                          NULL,
                          c_orig_first_cost,
                          c_load_status);

    p_load_status := SET_LOAD_STATUS(c_load_status);

    IF p_load_status = 'ERROR' THEN
        RAISE SAMPLE_ERROR;
    END IF;

    T_SP_RKP4290_REFITEM( p_entry_no,
                          p_owner,
                          'EID',
                          p_invoice_no,
                          l_ref_no,
                          p_item,     
                          p_line_no,
                          l_equip_id,
                          NULL,
                          NULL,
                          c_orig_first_cost, 
                          c_load_status);

    p_load_status := SET_LOAD_STATUS(c_load_status);

    IF p_load_status = 'ERROR' THEN
        RAISE SAMPLE_ERROR;
    END IF;

    p_line_no := p_line_no + 1;

  END LOOP L_0; 

  BEGIN
    UPDATE ENTRY_INV EI SET
        INV_ADJ_AMT = (SELECT NVL(SUM(CUSTOMS_VAL_2), 0) +
                              NVL(SUM(CUSTOMS_VAL_6), 0) +
                              NVL(SUM(CUSTOMS_VAL_7), 0)
                         FROM ENTRY_DTL 
                        WHERE OWNER      = p_owner
                          AND ENTRY_NO   = p_entry_no
                          AND INVOICE_NO = EI.INVOICE_NO),

        TOT_AMT = (SELECT NVL(SUM(CUSTOMS_VAL_1), 0) +
                          NVL(SUM(CUSTOMS_VAL_2), 0) +
                          NVL(SUM(CUSTOMS_VAL_6), 0) +
                          NVL(SUM(CUSTOMS_VAL_7), 0)
                     FROM ENTRY_DTL 
                    WHERE OWNER      = p_owner
                      AND ENTRY_NO   = p_entry_no
                      AND INVOICE_NO = EI.INVOICE_NO),

        INV_TOTAL_VAL = (SELECT NVL(SUM(CUSTOMS_VAL_1), 0) +
                                NVL(SUM(CUSTOMS_VAL_2), 0) +
                                NVL(SUM(CUSTOMS_VAL_6), 0) +
                                NVL(SUM(CUSTOMS_VAL_7), 0)
                           FROM ENTRY_DTL 
                          WHERE OWNER      = p_owner
                            AND ENTRY_NO   = p_entry_no
                            AND INVOICE_NO = EI.INVOICE_NO)

        WHERE OWNER      = p_owner
          AND ENTRY_NO   = p_entry_no
          AND INVOICE_NO = RTRIM(p_invoice_no);

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_SAMPLE', p_entry_no, '317', 'UPDATE', USER, 
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM, 
               'Entry Invoice No ' || p_invoice_no,
               'Error updating Entry Invoice for Non-Chargeable Sample',
               NULL, NULL
              );

        RAISE SAMPLE_ERROR;
  END; 



  EXCEPTION
    WHEN SAMPLE_ERROR THEN
      IF C_SAMPLE%ISOPEN THEN
          BEGIN
            CLOSE C_SAMPLE;

            EXCEPTION 
              WHEN OTHERS THEN NULL;
          END; 
      END IF;

    WHEN OTHERS THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_SAMPLE', p_entry_no, '318', 'UPDATE', USER, 
             'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM, 
             'B/L ' || p_bill_of_lading_no,
             'Unexpected SQL error in SAMPLE stored procedure',
             NULL, NULL
            );

      IF C_SAMPLE%ISOPEN THEN
          BEGIN
            CLOSE C_SAMPLE;

            EXCEPTION 
              WHEN OTHERS THEN NULL;
          END; 
      END IF;

END T_SP_RKP4290_SAMPLE;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_SAMPLE
** ************************************************************************* */

/
EXIT
