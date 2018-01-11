CREATE OR REPLACE PROCEDURE T_SP_RKP4290

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290.SQL                                             **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290                                            **
**                                                                           **
**  AUTHOR   : REDDY S CHILUKA                                               **
**  CREATED  : 17 DECEMBER 1999                                              **
**  PURPOSE  : This procedure should be executed from within the VB          **
**             application RKP4290. This procedure is to load the ENTRY      **
**             tables.                                                       **
**                                                                           **
** id_means_of_transp is named as trans_id                                   **
** ************************************************************************* */
(
   p_entry_no        IN     ENTRY_HDR.ENTRY_NO%TYPE,
   p_trans_id        IN     SHIPPING_HDR.ID_MEANS_OF_TRANSP%TYPE,
   p_movement_type   IN     SHIPPING_DTL.MOVEMENT_TYPE%TYPE,
   p_load_status     OUT    VARCHAR2,
   p_entry_port      IN     ENTRY_HDR.ENTRY_PORT%TYPE,
   p_cleared         IN     ENTRY_HDR.CUSTOMS_BROKER_ID%TYPE,
   p_cedate          IN     varchar2,
   p_owner           IN     ENTRY_INV.OWNER%TYPE

)

AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   c_other_costs_amt      COSTING_DTL.OTHER_COSTS_AMT%TYPE;

   c_import_date          ENTRY_HDR.IMPORT_DATE%TYPE;
   c_entry_port           ENTRY_HDR.ENTRY_PORT%TYPE;
   c_load_point           ENTRY_HDR.LOAD_POINT%TYPE;
   c_carrier              ENTRY_HDR.CARRIER%TYPE;
   c_entry_hmf_amt        ENTRY_HDR.ENTRY_HMF_AMT%TYPE;
   c_total_mpf            ENTRY_HDR.TOTAL_MPF%TYPE;
   c_total_value          ENTRY_HDR.TOTAL_VALUE%TYPE;
   c_total_duty           ENTRY_HDR.TOTAL_DUTY%TYPE;
   c_total_other          ENTRY_HDR.TOTAL_OTHER%TYPE;
   c_tot_duty_tax_fee     ENTRY_HDR.TOT_DUTY_TAX_FEE%TYPE;

   c_invoice_no           ENTRY_INV.INVOICE_NO%TYPE;
   c_invoice_no_wk        ENTRY_INV.INVOICE_NO%TYPE;
   c_invoice_no_temp      ENTRY_INV.INVOICE_NO%TYPE;
   c_temp_invoice_no      ENTRY_INV.INVOICE_NO%TYPE;
   c_manufacturer         ENTRY_INV.MANUFACTURER%TYPE;
   c_inv_total_val        ENTRY_INV.INV_TOTAL_VAL%TYPE;
   c_inv_adj_amt          ENTRY_INV.INV_ADJ_AMT%TYPE;
   c_bill_of_lading_no    ENTRY_INV.BILL_OF_LADING_NO%TYPE;
   c_p_link_key_val       ENTRY_INV.P_LINK_KEY_VAL%TYPE;

   c_item                 ENTRY_DTL.ITEM%TYPE;
   c_last_item            ENTRY_DTL.ITEM%TYPE;
   c_line_no              ENTRY_DTL.LINE_NO%TYPE;
   c_item_desc            ENTRY_DTL.ITEM_DESC%TYPE;
   c_ref_no               ENTRY_DTL.REF_NO%TYPE;
   c_qty_entered          ENTRY_DTL.QTY_ENTERED%TYPE;
   c_qty_um               ENTRY_DTL.QTY_UM%TYPE;
   c_item_duty_amt        ENTRY_DTL.ITEM_DUTY_AMT%TYPE;
   c_unit_duty            ENTRY_DTL.UNIT_DUTY%TYPE;
   c_origin_country       ENTRY_DTL.ORIGIN_COUNTRY%TYPE;
   c_origin_ctry_sav      ENTRY_DTL.ORIGIN_COUNTRY%TYPE;
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
   c_edi_ind              ENTRY_DTL.EDI_IND%TYPE;
   c_qty_avail_db         ENTRY_DTL.QTY_AVAIL_DB%TYPE;

   c_initial_date_1       EVENTS_DTL.INITIAL_DATE%TYPE;
   c_initial_date_2       EVENTS_DTL.INITIAL_DATE%TYPE;

   c_min_amount           EXPENSE.MIN_AMOUNT%TYPE;
   c_max_amount           EXPENSE.MAX_AMOUNT%TYPE;
   c_exp_add_to_duty_base EXPENSE.ADD_TO_DUTY_BASE%TYPE;

   c_supplier             ITEM_DTL.SUPPLIER%TYPE;
   c_item_gsp_status      ITEM_DTL.STATUS%TYPE;

   c_contract_ref_no      CONTRACT_HDR.CONTRACT_REF_NO%TYPE;
   c_contract_seller      CONTRACT_HDR.SELLER%TYPE;
   c_mc_hdr_seller        CONTRACT_HDR.SELLER%TYPE;

   c_expense_code         COSTING_DTL.EXPENSE_CODE%TYPE;
   c_expense_category     COSTING_DTL.EXPENSE_CATEGORY%TYPE;
   c_rec_id               COSTING_DTL.REC_ID%TYPE;

   c_inv_contract_no      INVOICE_DTL.CONTRACT_NO%TYPE;
   c_inv_price            INVOICE_DTL.PRICE%TYPE;
   c_inv_price_2          INVOICE_DTL.PRICE%TYPE;
   c_free_price           INVOICE_DTL.PRICE%TYPE;
   c_sample_price         INVOICE_DTL.PRICE%TYPE;
   c_discount_price       INVOICE_DTL.PRICE%TYPE;
   c_orig_vnd_price       INVOICE_DTL.PRICE%TYPE;
   c_invoice_line_no      INVOICE_DTL.LINE_NO%TYPE;
   c_seller               INVOICE_HDR.SELLER%TYPE;
   c_buyer                INVOICE_HDR.BUYER%TYPE;

   c_link_tbl_name        ADJUST_DTL.LINK_TBL_NAME%TYPE;
   c_link_key_val_1       ADJUST_DTL.LINK_KEY_VAL%TYPE;
   c_link_key_val_2       ADJUST_DTL.LINK_KEY_VAL%TYPE;
   c_discount_amt         ADJUST_DTL.ADJUST_AMOUNT%TYPE;
   c_discount_rate        ADJUST_DTL.VALUE%TYPE;

   c_adj_exp_code         ADJUST_DTL.EXPENSE_CODE%TYPE;
   c_adj_exp_category     ADJUST_DTL.EXPENSE_CATEGORY%TYPE;
   c_adj_basis            ADJUST_DTL.BASIS%TYPE;
   c_adj_value            ADJUST_DTL.VALUE%TYPE;
   c_adj_amt              ADJUST_DTL.ADJUST_AMOUNT%TYPE;
   c_vndr_disc_fxd_rate   ADJUST_DTL.VALUE%TYPE;
   c_vndr_disc_fxd_amt    ADJUST_DTL.ADJUST_AMOUNT%TYPE;
   c_vndr_disc_pct_rate   ADJUST_DTL.VALUE%TYPE;
   c_vndr_disc_pct_rate_2 ADJUST_DTL.VALUE%TYPE;
   c_vndr_disc_pct_amt    ADJUST_DTL.ADJUST_AMOUNT%TYPE;

/*   c_note_id              NOTES_HDR.NOTE_ID%TYPE;
   c_text                 NOTES_DTL.TEXT%TYPE;
   c_template_name        NOTES_TEMPL_HDR.TEMPLATE_NAME%TYPE;
*/
   c_hmf_ind              PLCI3225.HMF_IND%TYPE;

   c_component_desc       PRECLASS_DTL.ITEM_DESC%TYPE;
   c_export_country       PRECLASS_DTL.EXPORT_COUNTRY%TYPE;
   c_item_first_cost      PRECLASS_DTL.CUSTOMS_VAL%TYPE;
   c_hts_line_no          PRECLASS_DTL.LINE_NO%TYPE;

   c_transport_mode       SHIPPING_HDR.TRANSPORT_MODE%TYPE;
   c_final_dest           SHIPPING_HDR.FINAL_DEST%TYPE;
   c_voyage_number        SHIPPING_HDR.VOYAGE_NUMBER%TYPE;

   c_ship_contract_no     SHIPPING_DTL.CONTRACT_NO%TYPE;
   c_shipping_ref_no      SHIPPING_DTL.REF_NO%TYPE;
   c_ship_edi_ind         SHIPPING_DTL.EDI_IND%TYPE;
   c_ship_qty_avail_db    SHIPPING_DTL.QTY_AVAIL_DB%TYPE;
   c_ship_invoice_no      SHIPPING_DTL.INVOICE_NO%TYPE;
   c_ship_invoice_no_wk   SHIPPING_DTL.INVOICE_NO%TYPE;
   c_ship_invoice_no_tmp  SHIPPING_DTL.INVOICE_NO%TYPE;

   c_ship_free_ind        SHIPPING_DTL.EDI_IND%TYPE;

   c_qty_open_bal         TOOLING_HDR.QTY_OPEN_BAL%TYPE;
   c_duty_assess_base     TOOLING_HDR.DUTY_ASSESS_BASE%TYPE;
   t_edi_ind              TOOLING_DTL.EDI_IND%TYPE;

   nonchargeable_sample   VARCHAR2(6);
   c_tool_date_1          VARCHAR2(10);
   c_tool_date_2          VARCHAR2(12);
   c_med_check            VARCHAR2(12);
   c_load_status          VARCHAR2(10);

   c_date                 DATE;

   c_item_gsp_cnt         NUMBER;
   c_manipulator          NUMBER(2);
   c_temp                 NUMBER;
   c_notes_count          NUMBER;
   c_inv_dtl_count        NUMBER;
   c_refitem_count        NUMBER;
   c_seller_count         NUMBER;
   c_invoice_seller_cnt   NUMBER;
   c_adjusted_inv_price   NUMBER;
   c_invoice_dtl_count    NUMBER;
   c_entry_dtl_count      NUMBER;
   c_notes_dtl_count      NUMBER;
   c_sample_dtl_count     NUMBER;
   c_sql_ret_code         NUMBER;
   p3                     NUMBER;


   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */

   g_owner_aafw           CONSTANT SHIPPING_HDR.OWNER%TYPE          := 'AAFW';
   g_owner_rockblocks     CONSTANT SHIPPING_HDR.OWNER%TYPE          := 'ROCKBLOCKS';

   g_mc_ref_no_rwked      CONSTANT CONTRACT_HDR.REF_NO%TYPE         := 'REWORKED';

   g_edi_ind_f            CONSTANT SHIPPING_DTL.EDI_IND%TYPE        := 'F';
   g_edi_ind_m            CONSTANT SHIPPING_DTL.EDI_IND%TYPE        := 'M';
   g_edi_ind_s            CONSTANT SHIPPING_DTL.EDI_IND%TYPE        := 'S';
   g_edi_ind_x            CONSTANT SHIPPING_DTL.EDI_IND%TYPE        := 'X';
   g_edi_ind_y            CONSTANT SHIPPING_DTL.EDI_IND%TYPE        := 'Y';
   g_exp_code_mmvfoc      CONSTANT ADJUST_DTL.EXPENSE_CODE%TYPE     := 'MMVFOC';
   g_exp_code_mmvprct     CONSTANT ADJUST_DTL.EXPENSE_CODE%TYPE     := 'MMV%';
   g_exp_code_rwk         CONSTANT ADJUST_DTL.EXPENSE_CODE%TYPE     := 'REWORK';
   g_exp_code_discount    CONSTANT ADJUST_DTL.EXPENSE_CODE%TYPE     := 'DISCOUNT';
   g_exp_code_vndr_disc   CONSTANT ADJUST_DTL.EXPENSE_CODE%TYPE     := 'DISC-%';
   g_exp_cat_gds          CONSTANT ADJUST_DTL.EXPENSE_CATEGORY%TYPE := 'GDS';

   g_reference_qlf_ety    CONSTANT REFITEM.REFERENCE_QLF%TYPE       := 'ETY';
   g_reference_qlf_vfc    CONSTANT REFITEM.REFERENCE_QLF%TYPE       := 'VFC';
   g_refitem_ltn          CONSTANT REFITEM.LINK_TBL_NAME%TYPE       := 'SHIPPING - %';
   g_ltn_shipping         CONSTANT REFITEM.LINK_TBL_NAME%TYPE       := 'SHIPPING - ';
   g_refitem_last_user    CONSTANT REFITEM.LAST_USER%TYPE           := 'RKP4290VB';
   g_fl_entry             CONSTANT EVENTS_DTL.EVENT_ID%TYPE         := 'FL-ENTRY';
   g_fl_customs_desc      CONSTANT NOTES_DTL.NOTE_ID%TYPE           := 'FL-CUSTOMS-DESC';

   g_doc_msg_code_adr     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE       := 'ADR';
   g_doc_msg_code_cvd     CONSTANT SPECCOND.DOC_MSG_CODE%TYPE       := 'CVD';
   g_exp_cat_tax          CONSTANT EXPENSE.EXPENSE_CATEGORY%TYPE    := 'TAX';
   g_exp_code_mpf         CONSTANT EXPENSE.EXPENSE_CODE%TYPE        := 'MPF';

   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */

   NO_REFITEM_ROWS        EXCEPTION;
   PROCEDURE_ABORT        EXCEPTION;



/* ************************************************************************* **
**                 CURSORS                                                   **
** ************************************************************************* */

   CURSOR C_REFITEM IS
   SELECT UNIQUE LINK_KEY_VAL

     FROM REFITEM

     WHERE OWNER           IN (SELECT DISTINCT VALID_PARTY
                                 FROM PARTY_XREF
                                WHERE LINK_TBL_NAME LIKE 'PARTY - %'
                                  AND REFERENCE_QLF = 'OWN')
       AND LINK_TBL_NAME LIKE g_refitem_ltn
       AND REF_NO           = LTRIM(RTRIM(p_entry_no))
       AND REFERENCE_QLF    = g_reference_qlf_ety
       AND LAST_USER        = g_refitem_last_user;

/* ************************************************************************* */
   CURSOR C_SHIPPING_DTL_SAMPLE_INVOICE IS

   SELECT DISTINCT sd.INVOICE_NO
     FROM SHIPPING_DTL sd
    WHERE sd.OWNER             = p_owner
      AND sd.EDI_IND           = g_edi_ind_y
      AND sd.BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
    ORDER BY sd.INVOICE_NO;

/* ************************************************************************* */
   CURSOR C_SHIPPING_DTL_SAMPLE IS

   SELECT UNIQUE sd.ITEM,
                 sd.CONTRACT_NO,
                 sd.INVOICE_NO,
                 sd.ORIGIN_COUNTRY,
                 sd.EDI_IND,
                 r.ref_no

     FROM SHIPPING_DTL sd, REFITEM r

    WHERE sd.OWNER             = p_owner
      AND sd.BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
      AND sd.INVOICE_NO        = LTRIM(RTRIM(c_invoice_no))
      AND sd.EDI_IND           = g_edi_ind_y
      and r.owner(+)           = p_owner
      and r.link_tbl_name(+)   = 'SHIPPING - '|| sd.bill_of_lading_no
      and r.reference_qlf(+)   = 'MID'
      and r.link_key_val(+)    = sd.bill_of_lading_no || ' + ' ||
                                   sd.equip_id        || ' + ' ||
                                   sd.item            || ' + ' ||
                                   sd.ref_no
    ORDER BY sd.ITEM;

/* ************************************************************************* */

   CURSOR C_SHIPPING_DTL_REGULAR IS

   SELECT DISTINCT SD.ITEM,
                   SD.CONTRACT_NO,
                   SD.INVOICE_NO,
                   SD.ORIGIN_COUNTRY,
                   SD.QTY_AVAIL_DB,
                   SD.EDI_IND,
                   NVL(H.LINE_NO, 1.0),
                   CH.SELLER

     FROM SHIPPING_DTL SD,
          HTSBOOST_DTL H,
          CONTRACT_HDR CH

    WHERE SD.OWNER              = p_owner
      AND SD.BILL_OF_LADING_NO  = LTRIM(RTRIM(c_bill_of_lading_no))
      AND (   SD.EDI_IND != g_edi_ind_y
           OR SD.EDI_IND IS NULL)

      AND H.OWNER(+)            = SD.OWNER
      AND H.LINK_TBL_NAME(+)    = 'CONTRACT - '|| SD.CONTRACT_NO
      AND H.ITEM(+)             = SD.ITEM
      AND H.LINE_NO(+)          = 1.1

      AND CH.OWNER(+)           = SD.OWNER
      AND CH.CONTRACT_NO(+)     = SD.CONTRACT_NO

    ORDER BY SD.INVOICE_NO, NVL(H.LINE_NO, 1.0), SD.ITEM;

/* ************************************************************************* */
    CURSOR C_ROYALTY_CHECK  IS

    SELECT DISTINCT C.ITEM,
                    C.EXPENSE_CODE,
                    C.REC_ID,
                    C.EXPENSE_CATEGORY,
                    SD.CONTRACT_NO

      FROM COSTING_DTL C, EXPENSE EX, SHIPPING_DTL SD

     WHERE C.OWNER                = p_owner
       AND SD.OWNER               = p_owner
       AND SD.BILL_OF_LADING_NO   = c_bill_of_lading_no

       AND C.LINK_TBL_NAME        = 'CONTRACT - ' || SD.CONTRACT_NO
       AND C.ITEM                 = SD.ITEM
       AND C.EXPENSE_CATEGORY     = 'ROY'

       AND EX.EXPENSE_CATEGORY(+) = C.EXPENSE_CATEGORY
       AND EX.EXPENSE_CODE(+)     = C.EXPENSE_CODE
       AND EX.EXPENSE_CATEGORY    IS NULL;


/* ************************************************************************* */

   CURSOR C_INVOICE_DTL IS

   SELECT ID.ITEM,                ID.LINE_NO,         ID.CONTRACT_NO,
          NVL(ID.QTY_ORDERED, 0), ID.QTY_ORDERED_UM,  ID.PRICE + NVL(AD.VALUE, 0),
          ID.ITEM_EXT_AMT,        IH.SELLER,          NVL(IH.TOT_AMT, 0),
          ID.PRICE,               CH.CONTRACT_REF_NO,
      ABS( ROUND ( (NVL(ID.QTY_ORDERED, 0) * (ID.PRICE * (NVL(AD2.VALUE, 0) / 100) ) ), 3) ),
      ROUND (NVL(ID.PRICE, 0) * ( 1.0 + (NVL(AD2.VALUE, -100) / 100) ), 3),
      ID.PRICE,
      (NVL(AD2.VALUE, 0)) / 100

     FROM INVOICE_HDR IH, INVOICE_DTL ID, ADJUST_DTL AD, ADJUST_DTL AD2, CONTRACT_HDR CH

     WHERE IH.OWNER                 = p_owner
       AND IH.INVOICE_NO            = RTRIM(c_invoice_no)
       AND IH.SELLER               != g_owner_aafw
       AND IH.INV_REF_NO            = RTRIM(c_bill_of_lading_no)

       AND ID.OWNER                 = IH.OWNER
       AND ID.SELLER                = IH.SELLER
       AND ID.INVOICE_NO            = IH.INVOICE_NO
       AND ID.ITEM                  = RTRIM(c_item)
       AND ID.ORDER_NO              = IH.INV_REF_NO
       AND ID.CONTRACT_NO           = RTRIM(c_ship_contract_no)
       AND ID.PRICE                 = c_ship_qty_avail_db + ABS(NVL(AD.VALUE, 0))

       AND AD.OWNER (+)             = ID.OWNER
       AND AD.LINK_TBL_NAME (+)     = c_link_tbl_name || ID.SELLER
       AND AD.LINK_KEY_VAL (+)      = c_link_key_val_1 || ID.SELLER || c_link_key_val_2 ||
                                      ID.LINE_NO
       AND AD.EXPENSE_CODE (+)      = g_exp_code_rwk
       AND AD.EXPENSE_CATEGORY (+)  = g_exp_cat_gds
       AND NVL(AD.VALUE (+), 0)     < 0

       AND AD2.OWNER (+)            = ID.OWNER
       AND AD2.LINK_TBL_NAME (+)    = c_link_tbl_name || ID.SELLER
       AND AD2.LINK_KEY_VAL (+)     = c_link_key_val_1 || ID.SELLER || c_link_key_val_2 ||
                                       ID.LINE_NO
       AND AD2.EXPENSE_CODE (+)     = g_exp_code_discount
       AND AD2.EXPENSE_CATEGORY (+) = g_exp_cat_gds
       AND NVL(AD2.VALUE (+), 0)    < 0

       AND CH.OWNER (+)             = ID.OWNER
       AND CH.CONTRACT_NO (+)       = ID.CONTRACT_NO
     ORDER BY IH.INVOICE_NO, ID.ITEM;


/* ************************************************************************* */
    CURSOR C_VNDR_DISCOUNT  IS

    SELECT DISTINCT A.EXPENSE_CODE,
                    A.EXPENSE_CATEGORY,
                    A.BASIS,
                    NVL(A.VALUE, 0),
                    NVL(A.ADJUST_AMOUNT, 0)

      FROM ADJUST_DTL  A

     WHERE A.OWNER                         = p_owner
       AND A.LINK_TBL_NAME                 = 'INVOICE - ' || c_invoice_no || ' + ' || c_seller
       AND A.LINK_KEY_VAL                  = c_invoice_no || ' + '  || c_seller || ' + ' || c_bill_of_lading_no ||
                                                    ' + ' || c_item || ' + ' || c_invoice_line_no
       AND A.EXPENSE_CODE               LIKE g_exp_code_vndr_disc
       AND A.EXPENSE_CATEGORY              = g_exp_cat_gds
       AND ABS(NVL(A.ADJUST_AMOUNT, 0))   != 0;


/* ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL FUNCTION SET_LOAD_STATUS

             Load Status translation for end users
               ERROR - Shipments Clumped was unsuccessful (Contact RSTS)
               FALSE - Shipments Clumped was unsuccessful (Check error rpt)
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
            ('T_SP_RKP4290', p_entry_no, '001', 'IF', USER,
             'CE', '1', 'ERR', NULL,
             'Internal Stored Procedure Error',
             'Load status of ' || l_status || ' is invalid',
             'Load status must be WARN, FALSE, ERROR or TRUE',
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
             BEGIN OF LOCAL STORED PROCEDURE INSERT_TOOLING_DTL
** ************************************************************************* */

PROCEDURE INSERT_TOOLING_DTL
   (
    l_manifest_qty      IN  TOOLING_DTL.MANIFEST_QTY%TYPE,
    l_qty_entered       IN  TOOLING_DTL.QTY_ENTERED%TYPE,
    l_sql_ret_code      OUT NUMBER
   )

IS

BEGIN
  INSERT INTO TOOLING_DTL
         (OWNER,
          TOOLING_NO,
          TOOLING_REQD_DATE,
          MANUFACTURER,
          ITEM,
          MANIFEST_QTY,
          ENTRY_QTY_UM,
          QTY_ENTERED,
          MANIFEST_QTY_UM,
          BILL_OF_LADING_NO,
          P_LINK_KEY_VAL,
          LAST_USER,
          LAST_UPDATE,
          LAST_ACTIVITY
         )

   VALUES(g_owner_aafw,
          c_item,
          TO_DATE(c_tool_date_2, 'MMDDYYYYHH24MI'),
          c_buyer,
          c_item,
          l_manifest_qty,
          'PC',
          l_qty_entered,
          'PC',
          c_bill_of_lading_no,
          c_item ||' + '|| TO_CHAR(SYSDATE, 'MMDDYY')
                         ||' + '|| c_buyer ||' + '|| c_item,
          'RKP4290',
          SYSDATE,
          'A'
         );

   l_sql_ret_code := SQLCODE;


   EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
         l_sql_ret_code := SQLCODE;

      WHEN OTHERS THEN
         l_sql_ret_code := SQLCODE;
         p_load_status := SET_LOAD_STATUS('FALSE');

         p3 := shrmsglog_pkg.snd_log_msg
               ('T_SP_RKP4290', p_entry_no, '002', 'INSERT',
                USER, 'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                'B/L '|| c_bill_of_lading_no,
                'Tooling Date - '|| c_tool_date_2,
                'Item ' || c_item,
                'An SQL error while creating Tooling detail.'
                );
END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE INSERT_TOOLING_DTL
** ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL STORED PROCEDURE INCREMENT_TOOLING_DATE
** ************************************************************************* */

PROCEDURE INCREMENT_TOOLING_DATE IS
   c_work_hour            NUMBER;
   c_work_min             NUMBER;
   c_work_date            VARCHAR2(8);

BEGIN
   c_work_date := NULL;
   c_work_date := SUBSTR(c_tool_date_2, 1, 8);

   c_work_hour := TO_NUMBER(SUBSTR(c_tool_date_2, 9, 2));
   c_work_min  := TO_NUMBER(SUBSTR(c_tool_date_2, 11, 2));

   c_work_min  := c_work_min + 1;

   IF c_work_min > 59 THEN
       c_work_min    := 01;
       c_work_hour   := c_work_hour + 1;

       IF c_work_hour > 24 THEN
           c_work_hour := 00;
       END IF;

   END IF;

   c_tool_date_2 := NULL;

   c_tool_date_2 := c_work_date || LPAD(TO_CHAR(c_work_hour), 2, '0')
                    || LPAD(TO_CHAR(c_work_min), 2, '0');

END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE INCREMENT_TOOLING_REQD_DATE
** ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL FUNCTION UPDATE_TOOLING_HDR
** ************************************************************************* */

FUNCTION  UPDATE_TOOLING_HDR
   (
    l_qty_open_bal      IN  TOOLING_HDR.QTY_OPEN_BAL%TYPE,
    l_tooling_no        IN  TOOLING_HDR.TOOLING_NO%TYPE
   )
   RETURN NUMBER

IS

BEGIN
  UPDATE TOOLING_HDR

     SET QTY_OPEN_BAL  = l_qty_open_bal,
         LAST_UPDATE   = SYSDATE,
         LAST_ACTIVITY = 'C',
         LAST_USER     = 'RKP4290'

   WHERE OWNER      = g_owner_aafw
     AND TOOLING_NO = l_tooling_no;

   RETURN(SQLCODE);

   EXCEPTION
      WHEN OTHERS THEN
         p_load_status := SET_LOAD_STATUS('FALSE');

         p3 := shrmsglog_pkg.snd_log_msg
               ('T_SP_RKP4290', p_entry_no, '003', 'UPDATE',
                USER, 'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                'Item ' || c_item,
                'Open Qty  Balance ' || l_qty_open_bal,
                'Error updating Tooling Header.',
                NULL
                );

         RETURN(SQLCODE);
END;

/* ************************************************************************* **
             END OF LOCAL FUNCTION UPDATE_TOOLING_HDR
** ************************************************************************* */



/* ************************************************************************* **
             BEGIN OF LOCAL FUNCTION GET_MAX_LINE_NO
** ************************************************************************* */

FUNCTION  GET_MAX_LINE_NO
   (
    l_owner           IN  ENTRY_DTL.OWNER%TYPE,
    l_entry_no        IN  ENTRY_DTL.ENTRY_NO%TYPE,
    l_invoice_no      IN  ENTRY_DTL.INVOICE_NO%TYPE
   )
   RETURN NUMBER

IS

   line_no                ENTRY_DTL.LINE_NO%TYPE;

BEGIN

  SELECT NVL(MAX(TRUNC(LINE_NO)),0) + 1
    INTO line_no

    FROM ENTRY_DTL

   WHERE OWNER      = LTRIM(RTRIM(l_owner))
     AND ENTRY_NO   = LTRIM(RTRIM(l_entry_no))
     AND INVOICE_NO = LTRIM(RTRIM(l_invoice_no));

   RETURN(line_no);

END;

/* ************************************************************************* **
             END OF LOCAL FUNCTION GET_MAX_LINE_NO
** ************************************************************************* */


/* ************************************************************************* **
                 BEGIN OF LOCAL STORED PROCEDURE VNDR_DISCOUNT
** ************************************************************************* */

PROCEDURE VNDR_DISCOUNT

IS

   VNDR_DISC_ABORT        EXCEPTION;


BEGIN
  BEGIN
    OPEN C_VNDR_DISCOUNT;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '004', 'OPEN', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Ltn ' || c_link_tbl_name || c_seller,
                 'Lkv ' || c_link_key_val_1 || c_seller || c_link_key_val_2 || c_invoice_line_no,
                 'Exp Cat ' || g_exp_cat_gds || ', Exp Code Like ' || g_exp_code_vndr_disc,
                 'Error in Opening Vendor Discount cursor'
                 );

       RAISE VNDR_DISC_ABORT;
  END;


  c_vndr_disc_fxd_rate   := 0;
  c_vndr_disc_fxd_amt    := 0;
  c_vndr_disc_pct_rate   := 0;
  c_vndr_disc_pct_rate_2 := 0;
  c_vndr_disc_pct_amt    := 0;


  <<L_2>>
  LOOP
    c_adj_exp_code      := NULL;
    c_adj_exp_category  := NULL;
    c_adj_basis         := NULL;
    c_adj_value         := 0;
    c_adj_amt           := 0;

    BEGIN
      FETCH C_VNDR_DISCOUNT
       INTO c_adj_exp_code,
            c_adj_exp_category,
            c_adj_basis,
            c_adj_value,
            c_adj_amt;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '005', 'FETCH', USER,
                   'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                   'Ltn ' || c_link_tbl_name || c_seller,
                   'Lkv ' || c_link_key_val_1 || c_seller || c_link_key_val_2 || c_invoice_line_no,
                   'Exp Cat ' || g_exp_cat_gds || ', Exp Code Like ' || g_exp_code_vndr_disc,
                   'Error retrieving from Vendor Discount cursor'
                  );

          RAISE VNDR_DISC_ABORT;
    END;

    EXIT L_2 WHEN C_VNDR_DISCOUNT%NOTFOUND;

    BEGIN
      SELECT NVL(ADD_TO_DUTY_BASE, 'X')
        INTO c_exp_add_to_duty_base
        FROM EXPENSE
       WHERE OWNER            = g_owner_rockblocks
         AND EXPENSE_CODE     = c_adj_exp_code
         AND EXPENSE_CATEGORY = c_adj_exp_category;

      EXCEPTION

        WHEN NO_DATA_FOUND THEN
          c_exp_add_to_duty_base := NULL;

          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '006', 'SELECT', USER,
                 'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item || ', Invoice ' || c_invoice_no,
                 'Vndr Disc Exp Code ' || c_adj_exp_code || ', Exp Cat ' ||
                       c_adj_exp_category || ' not in Expense table'
                );

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '007', 'SELECT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item || ', Inv ' || c_invoice_no,
                 'Error selecting Vndr Disc Exp Code ' || c_adj_exp_code || ' from Expense'
                );

          RAISE VNDR_DISC_ABORT;

      END;

      IF ( c_exp_add_to_duty_base = 'I' ) THEN

          IF ( c_adj_basis = '$' ) THEN
               c_vndr_disc_fxd_rate :=  c_vndr_disc_fxd_rate + c_adj_value;
               c_vndr_disc_fxd_amt  :=  c_vndr_disc_fxd_amt  + c_adj_amt;

              IF ( c_orig_vnd_price != 0 ) THEN
                  c_vndr_disc_pct_rate_2 := c_vndr_disc_pct_rate_2 +
                                                 (c_adj_value / c_orig_vnd_price);
              END IF;

          ELSIF ( c_adj_basis = '%' ) THEN
               c_vndr_disc_pct_rate := c_vndr_disc_pct_rate + c_adj_value;
               c_vndr_disc_pct_amt  := c_vndr_disc_pct_amt  + c_adj_amt;

          END IF;

      END IF;

    END LOOP L_2;

    BEGIN
      CLOSE C_VNDR_DISCOUNT;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '008', 'CLOSE', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Ltn ' || c_link_tbl_name || c_seller,
                 'Lkv ' || c_link_key_val_1 || c_seller || c_link_key_val_2 || c_line_no,
                 'Exp Cat ' || g_exp_cat_gds || ', Exp Code Like ' || g_exp_code_vndr_disc,
                 'Error in Closing Vendor Discount cursor'
                 );

         RAISE VNDR_DISC_ABORT;
    END;

  EXCEPTION
    WHEN VNDR_DISC_ABORT THEN
      IF C_VNDR_DISCOUNT%ISOPEN THEN
          BEGIN
            CLOSE C_VNDR_DISCOUNT;

            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

      RAISE PROCEDURE_ABORT;

END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE VNDR_DISCOUNT
** ************************************************************************* */


/* ************************************************************************* **
                 BEGIN OF LOCAL STORED PROCEDURE REGULAR_ITEMS
** ************************************************************************* */

PROCEDURE REGULAR_ITEMS IS

   REGULAR_ITEM_ERROR    EXCEPTION;

BEGIN
/** ************************************************************************* **
 **   Checking for multiple sellers on the same Vendor Invoice number.        **
 ** ************************************************************************* */
  IF c_temp_invoice_no != c_invoice_no THEN
      BEGIN

        c_invoice_seller_cnt := 0;

        SELECT COUNT(1) into c_invoice_seller_cnt

          FROM INVOICE_HDR

         WHERE OWNER       = p_owner
           AND INVOICE_NO  = LTRIM(RTRIM(c_invoice_no))
           AND SELLER     != g_owner_aafw
           AND INV_REF_NO  = LTRIM(RTRIM(c_bill_of_lading_no));

        EXCEPTION

          WHEN OTHERS THEN
            p_load_status := 'ERROR';

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '009', 'SELECT', USER,
                   'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                   'Invoice No ' || c_invoice_no,
                   'B/L ' || c_bill_of_lading_no,
                   'Error counting Sellers on Invoice Header',
                   NULL
                  );

            RAISE REGULAR_ITEM_ERROR;
      END;

  END IF;

/* ************************************************************************* */

  BEGIN
    c_link_tbl_name  := 'INVOICE - ' || c_invoice_no || ' + ';

    c_link_key_val_1 := c_invoice_no || ' + ';
    c_link_key_val_2 := ' + ' || c_bill_of_lading_no || ' + ' || c_item || ' + ';

    OPEN C_INVOICE_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '010', 'OPEN', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || c_bill_of_lading_no,
               'Contract ' || c_ship_contract_no,
               'Seller ' || c_seller || ' Item ' || c_item,
               'Error Opening Invoice Detail cursor for Invoice '
                    || c_invoice_no
               );

        RAISE REGULAR_ITEM_ERROR;
  END;

/* ************************************************************************* */

  c_invoice_dtl_count    := 0;
  c_vndr_disc_pct_rate   := 0;
  c_vndr_disc_pct_rate_2 := 0;

  <<L_3>>
  LOOP
    c_qty_entered        := 0;
    c_qty_um             := NULL;
    c_customs_val_1      := 0;
    c_invoice_line_no    := 0;
    c_seller             := NULL;
    c_inv_total_val      := 0;
    c_manufacturer       := NULL;
    c_ship_free_ind      := NULL;
    c_free_price         := 0;
    c_contract_ref_no    := NULL;
    c_discount_amt       := 0;
    c_discount_price     := 0;
    c_orig_vnd_price     := 0;
    c_discount_rate      := 0;
    c_adjusted_inv_price := 0;

    BEGIN
      FETCH C_INVOICE_DTL
       INTO c_item,           c_invoice_line_no, c_inv_contract_no,
            c_qty_entered,    c_qty_um,          c_inv_price,
            c_customs_val_1,  c_seller,          c_inv_total_val,
            c_inv_price_2,    c_contract_ref_no, c_discount_amt,
            c_discount_price, c_orig_vnd_price,  c_discount_rate;


      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '011', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item,
                 'Error selecting Invoice dtl data from Invoice '
                    || c_invoice_no
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

    BEGIN
      SELECT NVL(SUM(NVL(VALUE, 0)), 0)
        INTO c_adjusted_inv_price
        FROM ADJUST_DTL

       WHERE OWNER (+)               = p_owner
         AND LINK_TBL_NAME (+)       = c_link_tbl_name || c_seller
         AND LINK_KEY_VAL (+)     LIKE c_link_key_val_1 || c_seller || c_link_key_val_2
         AND EXPENSE_CODE (+)     LIKE 'RE%'
         AND EXPENSE_CATEGORY (+)    = g_exp_cat_gds;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '012', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item,
                 'Error adjustment dtl data from Invoice '
                    || c_invoice_no
                );

          RAISE REGULAR_ITEM_ERROR;

    END;


    IF ( c_discount_rate != 0 ) THEN
        c_adjusted_inv_price := c_discount_price;
    ELSE
        c_adjusted_inv_price := c_adjusted_inv_price + c_orig_vnd_price;
    END IF;

    IF (c_seller IS NOT NULL) THEN
        VNDR_DISCOUNT( );
    END IF;

    IF (c_vndr_disc_pct_rate_2 != 0) THEN
        c_adjusted_inv_price := c_adjusted_inv_price + (c_orig_vnd_price * c_vndr_disc_pct_rate_2);
        c_discount_rate      := c_discount_rate + (NVL(c_vndr_disc_pct_rate_2, 0) /100);
    END IF;

    IF ( c_orig_vnd_price != 0 AND (c_vndr_disc_pct_rate_2 != 0 OR c_vndr_disc_pct_rate != 0)) THEN
        c_adjusted_inv_price := c_adjusted_inv_price +
                                 TRUNC( (c_orig_vnd_price * (NVL( c_vndr_disc_pct_rate, -100) / 100) ), 3 );

        c_discount_rate      := ROUND ( (c_discount_rate + (NVL(c_vndr_disc_pct_rate, 0) / 100)), 3);

        c_discount_price     := ROUND ( NVL(c_adjusted_inv_price, 0), 3);

        c_discount_amt       := ABS( ROUND( NVL(c_adjusted_inv_price, 0), 3)
                                    * NVL(c_qty_entered, 0) );

        /* ******************************************************************************************* */
        /*   Kavin 1/11/2011:  Due to rounding, the vendor discount amount in the adjustment booster   */
        /*          attached to the vendor invoice may not match the clumped vendor discount amount.   */
        /*          Rockport is using three decimal places for calculating the Vendor Discount Amounts */
        /*          in the adjustment booster at vendor invoice detail level.  For customs clearance,  */
        /*          the adjusted invoice price is being rounded to two decimal places before the       */
        /*          extended amount is calculated.                                                     */
        /*          Therefore the ((original vendor invoice price multiplied by the clumped qty) minus */
        /*          (discounted vendor price multiplied by the clumped quantity)) may not match the    */
        /*          the vendor discount amount stated in the vendor discount adjustment booster at     */
        /*          vendor invoice detail level.                                                       */
        /*          If everything was carried to three decimal places then rounding would NOT be an    */
        /*          issue.  I discussed issue with LouAnn Dunsworth. For now this is OK.               */
        /* ******************************************************************************************* */
        /* ******************************************************************************************* */
        /*   Kavin 5/16/2011:  Value stored in entry_dtl.rate_1 will now be carried out to three       */
        /*          decimal places instead of two.  Change made to address rounding issue discovered   */
        /*          by Impromptu report RKP4946.   Change requested by Audrey Mansell.                 */
        /* ******************************************************************************************* */

    END IF;

    c_adjusted_inv_price := ROUND(c_adjusted_inv_price, 3);

    c_invoice_dtl_count := C_INVOICE_DTL%ROWCOUNT;


    EXIT L_3 WHEN C_INVOICE_DTL%NOTFOUND;


    c_manufacturer := LTRIM(RTRIM(c_seller));

    IF (c_invoice_seller_cnt > 1) THEN
        c_invoice_no_wk := LTRIM(RTRIM(c_invoice_no)) || '-' || c_manufacturer;
    ELSE
        c_invoice_no_wk := LTRIM(RTRIM(c_invoice_no));
    END IF;

    IF (NVL(c_contract_ref_no, 'X') != g_mc_ref_no_rwked) THEN
        c_inv_price := c_inv_price_2;
    END IF;


/* ************************************************************************* */
    IF c_temp_invoice_no != c_invoice_no_wk THEN
        BEGIN
          c_line_no := GET_MAX_LINE_NO(p_owner, p_entry_no, c_invoice_no_wk);

          c_p_link_key_val := NULL;

          c_p_link_key_val := p_entry_no || ' + ' || c_invoice_no_wk;

          INSERT INTO ENTRY_INV
                  (OWNER, ENTRY_NO, INVOICE_NO, MANUFACTURER, INV_TOTAL_VAL,
                   REFERENCE_CURRENCY, ROE, INVOICE_DESC, ADJUST_VALUE,
                   BILL_OF_LADING_NO, LAST_USER, LAST_UPDATE, LAST_ACTIVITY,
                   P_LINK_KEY_VAL
                  )

            VALUES(p_owner, p_entry_no, c_invoice_no_wk, c_manufacturer,
                   c_inv_total_val, 'US', '1', 'Electrical Equipment', 'N',
                   c_bill_of_lading_no, 'RKP4290', SYSDATE, 'A',
                   c_p_link_key_val
                  );

          EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
              NULL;

            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '013', 'INSERT', USER,
                     'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'Entry Invoice No ' || c_invoice_no,
                     'Error adding Chargeable Item to Entry Invoice',
                     NULL, NULL
                    );

              RAISE REGULAR_ITEM_ERROR;
        END;

        c_temp_invoice_no := c_invoice_no_wk;

    END IF;

    IF (c_contract_seller != c_seller ) THEN
        p_load_status := SET_LOAD_STATUS('FALSE');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '014', 'IF', USER, 'CF',
               TO_CHAR(SQLCODE), 'ERR', NULL,
               'B/L '|| c_bill_of_lading_no,
               'Invoice '|| c_invoice_no,
               'Contract '|| c_inv_contract_no ||', Item ' || c_item,
               'Vndr Mismatch - Contract: '|| RTRIM(c_contract_seller)
                    ||', Invoice: ' || c_supplier
               );
    END IF;


/* ************************************************************************* */

    BEGIN
      SELECT NVL(SUM(NVL(QTY, 0)), 0) 

        INTO c_qty_entered

        FROM SHIPPING_DTL

       WHERE OWNER                      = p_owner
         AND BILL_OF_LADING_NO          = LTRIM(RTRIM(c_bill_of_lading_no))
         AND ITEM                       = LTRIM(RTRIM(c_item))
         AND CONTRACT_NO                = LTRIM(RTRIM(c_ship_contract_no))
         AND INVOICE_NO                 = LTRIM(RTRIM(c_invoice_no))
         AND ORIGIN_COUNTRY             = LTRIM(RTRIM(c_origin_country))
         AND QTY_AVAIL_DB               = c_inv_price
         AND NVL(EDI_IND, g_edi_ind_x) != g_edi_ind_y;

      EXCEPTION
        WHEN OTHERS THEN
          c_item_desc   := NULL;
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '015', 'SELECT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no || ', Contract ' ||
                       c_ship_contract_no,
                 'Item - ' || c_item || ', Origin Country ' ||
                       c_origin_country,
                 'Error selecting total qty from SHIPPING_DTL'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

    c_ref_no        := LTRIM(RTRIM(c_inv_contract_no));
    c_customs_val_1 := c_inv_price_2 * c_qty_entered;

/* ************************************************************************* */

    IF c_inv_price_2 = 0 THEN
        BEGIN
          c_link_tbl_name := 'SHIPPING - ' || c_bill_of_lading_no;

          SELECT UNIQUE TO_NUMBER(REF_NO)

            INTO c_free_price

            FROM REFITEM

           WHERE OWNER         = p_owner
             AND LINK_TBL_NAME = c_link_tbl_name
             AND LINK_KEY_VAL  = (SELECT BILL_OF_LADING_NO || ' + ' ||
                                         EQUIP_ID          || ' + ' ||
                                         ITEM              || ' + ' ||
                                         REF_NO

                                    FROM SHIPPING_DTL

                                   WHERE OWNER              = p_owner
                                     AND BILL_OF_LADING_NO  = LTRIM(RTRIM(c_bill_of_lading_no))
                                     AND ITEM               = LTRIM(RTRIM(c_item))
                                     AND CONTRACT_NO        = LTRIM(RTRIM(c_ship_contract_no))
                                     AND INVOICE_NO         = LTRIM(RTRIM(c_invoice_no))
                                     AND ORIGIN_COUNTRY     = LTRIM(RTRIM(c_origin_country))
                                     AND QTY_AVAIL_DB       = c_inv_price_2
                                     AND NVL(EDI_IND, g_edi_ind_x) IN (g_edi_ind_f, g_edi_ind_m, g_edi_ind_s)
                                     AND ROWNUM             = 1
                                 )

             AND REFERENCE_QLF = g_reference_qlf_vfc;


          IF (c_free_price != 0) THEN
              c_customs_val_1 := c_free_price * c_qty_entered;
              c_ship_free_ind := g_edi_ind_y;
          END IF;


          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              c_free_price := 0;

            WHEN OTHERS THEN
              c_item_desc   := NULL;
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290', p_entry_no, '016', 'SELECT', USER,
                      'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                      'B/L '|| c_bill_of_lading_no,
                      'Invoice '|| c_invoice_no || ', Contract ' ||
                            c_ship_contract_no,
                      'Item - ' || c_item || ', Origin Country ' ||
                            c_origin_country,
                      'Error selecting ' || g_reference_qlf_vfc || ' for free item from REFITEM'
                     );

              RAISE REGULAR_ITEM_ERROR;
        END;

    END IF;

/* ************************************************************************* */

    BEGIN
      IF (c_free_price = 0 AND c_discount_amt != 0) THEN
          c_customs_val_1 := ROUND((c_discount_price * c_qty_entered), 3);
      ELSE
          c_orig_vnd_price := 0;
          c_discount_rate  := 0;
      END IF;
    END;

/* ************************************************************************* */


    c_customs_val_3 := 0;

    IF c_ship_edi_ind = 'R' THEN
        BEGIN
          c_link_tbl_name := 'INVOICE - ' || LTRIM(RTRIM(c_invoice_no)) ||' + '||
                               LTRIM(RTRIM(c_seller));

          c_link_key_val_1 :=  LTRIM(RTRIM(c_invoice_no)) ||' + '||
                                  LTRIM(RTRIM(c_seller)) ||' + '||
                                  LTRIM(RTRIM(c_bill_of_lading_no))
                                  ||' + '||
                                  LTRIM(RTRIM(c_item)) ||' + '||
                                  LTRIM(RTRIM(c_invoice_line_no));

          SELECT ABS(NVL(SUM(NVL(ADJUST_AMOUNT, 0)), 0)) INTO c_customs_val_3

            FROM ADJUST_DTL

           WHERE OWNER            = p_owner
             AND LINK_TBL_NAME    = c_link_tbl_name
             AND LINK_KEY_VAL     = c_link_key_val_1
             AND EXPENSE_CODE     = g_exp_code_rwk
             AND EXPENSE_CATEGORY = g_exp_cat_gds;

          c_customs_val_1 := c_customs_val_1 - c_customs_val_3;

          IF c_customs_val_3 = 0 THEN
              p_load_status := SET_LOAD_STATUS('WARN');

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '017', 'IF', USER,
                     'CW', '1', 'INF', NULL,
                     'B/L  '|| c_bill_of_lading_no,
                     'Invoice '|| c_invoice_no,
                     'Item ' || c_item,
                     'Rework Amount is Zero in Adjustment Booster ' ||
                           'off Invoice'
                    );
          END IF;


          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              c_customs_val_3 := 0;

            WHEN OTHERS THEN
              c_item_desc   := NULL;
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '018', 'SELECT', USER,
                     'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'B/L  '|| c_bill_of_lading_no,
                     'Invoice '|| c_invoice_no,
                     'Item ' || c_item,
                     'Error Select Rework Adjustment Amt. Check '||
                           'ADJUST_DTL booster off INVOICE'
                    );

              RAISE REGULAR_ITEM_ERROR;
        END;
    END IF;

/* ************************************************************************* */

    BEGIN
      c_item_desc     := NULL;
      c_link_tbl_name := 'ITEM - ' || LTRIM(RTRIM(c_item));

      SELECT TEXT INTO c_item_desc

        FROM NOTES_DTL

       WHERE OWNER         = p_owner
         AND LINK_TBL_NAME = c_link_tbl_name
         AND LINK_KEY_VAL  = LTRIM(RTRIM(c_item))
         AND NOTE_ID       = g_fl_customs_desc
         AND ROWNUM        = 1

       ORDER BY LINE_NO;


      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          BEGIN
            c_item_desc := NULL;

            SELECT ITEM_DESC INTO c_item_desc

              FROM ITEM_HDR

             WHERE OWNER = p_owner
               AND ITEM  = LTRIM(RTRIM(c_item))
               AND RTRIM(ITEM_DESC) IS NOT NULL;

            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                c_item_desc   := NULL;
                p_load_status := SET_LOAD_STATUS('FALSE');

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '019', 'SELECT', USER,
                       'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'B/L '|| c_bill_of_lading_no,
                       'Invoice '|| c_invoice_no || ' Item ' || c_item,
                       'Customs description is missing. Update ' ||
                          'ITEM_HDR for item description or',
                       'Notes Booster (' || g_fl_customs_desc || ') off Item.'
                      );

              WHEN OTHERS THEN
                c_item_desc   := NULL;
                p_load_status := 'ERROR';

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '020', 'SELECT', USER,
                       'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'B/L '|| c_bill_of_lading_no,
                       'Invoice '|| c_invoice_no,
                       'Item ' || c_item,
                       'Error selecting Customs description. Check ' ||
                          'ITEM_HDR for ITEM_DESCRIPTION.'
                      );

                RAISE REGULAR_ITEM_ERROR;
          END;

        WHEN OTHERS THEN
          c_item_desc := NULL;
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '021', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no || ' Item ' || c_item,
                 'Error getting Customs description.',
                 'Check the Notes Booster (' || g_fl_customs_desc || ') off Item.'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      SELECT TO_CHAR(c_import_date,'MMDDYYYY')||
                   LPAD(TO_CHAR(SYSDATE,'HH24'), 2, '0'),
             TO_NUMBER(TO_CHAR(SYSDATE, 'MI'))

        INTO c_tool_date_1, c_manipulator

        FROM DUAL;

      c_manipulator := c_manipulator + 1;

      IF (c_manipulator > 59) THEN
          c_manipulator := 10;
      END IF;

      c_tool_date_2 := c_tool_date_1 || TO_CHAR(c_manipulator);

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_tool_date_2 := c_import_date;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '022', 'SELECT', USER, 'CF',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item,
                 'Tooling date manipulation error.'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_qty_open_bal     := 0;
      c_duty_assess_base := 0;
      t_edi_ind          := NULL;

      SELECT NVL(QTY, 0), DUTY_ASSESS_BASE, EDI_IND
        INTO c_qty_open_bal, c_duty_assess_base, t_edi_ind

        FROM TOOLING_HDR
       WHERE OWNER      = g_owner_aafw
         AND TOOLING_NO = c_item;

      SELECT c_qty_open_bal - NVL(SUM(MANIFEST_QTY), 0)
        INTO c_qty_open_bal
        FROM TOOLING_DTL
       WHERE OWNER      = g_owner_aafw
         AND TOOLING_NO = c_item;

      IF c_qty_entered > c_qty_open_bal THEN
          IF c_qty_open_bal > 0 THEN
              LOOP
                INSERT_TOOLING_DTL(c_qty_open_bal, c_qty_open_bal,
                                   c_sql_ret_code);

                EXIT WHEN c_sql_ret_code = 0;

                IF c_sql_ret_code = -1 THEN
                    INCREMENT_TOOLING_DATE( );
                ELSE
                    RAISE REGULAR_ITEM_ERROR;
                END IF;
              END LOOP;

/***************************************************************************/

              IF UPDATE_TOOLING_HDR(0, c_item) != 0 THEN
                  RAISE REGULAR_ITEM_ERROR;
              END IF;

/***************************************************************************/

              IF t_edi_ind = 'S' THEN
                  c_customs_val_2 := c_duty_assess_base * c_qty_open_bal;
              END IF;

              p_load_status := SET_LOAD_STATUS('WARN');

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '023', 'COMPARE', USER,
                     'CW', '1', 'INF', NULL,
                     'B/L '|| c_bill_of_lading_no,
                     'Invoice '|| c_invoice_no,
                     'Item ' || c_item,
                     'Die charge quantity is greater than the open balance'
                    );

          END IF;  /*  IF c_qty_open_bal > 0 */
      ELSE
          IF t_edi_ind = 'S' THEN
              c_customs_val_2 := c_duty_assess_base * c_qty_entered;
          END IF;

/****************************************************************************/

          LOOP
            INSERT_TOOLING_DTL(c_qty_entered, c_qty_entered,
                               c_sql_ret_code);

            EXIT WHEN c_sql_ret_code = 0;

            IF c_sql_ret_code = -1 THEN
                INCREMENT_TOOLING_DATE;
            ELSE
                RAISE REGULAR_ITEM_ERROR;
            END IF;
          END LOOP;

/****************************************************************************/

          IF UPDATE_TOOLING_HDR(c_qty_open_bal - c_qty_entered,c_item) != 0
          THEN
              RAISE REGULAR_ITEM_ERROR;
          END IF;

/****************************************************************************/

      END IF;    /* for IF c_qty_entered > c_qty_open_bal      */


      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_customs_val_2 := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '024', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Tooling Date '|| c_tool_date_2,
                 'Item ' || c_item,
                 'Error selecting Tooling information'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_rate_4 := 0;

      SELECT NVL(RATE_PER_UNIT_1 * c_customs_val_1, 0)
        INTO c_rate_4

        FROM EXPENSE

       WHERE EXPENSE_CATEGORY = g_exp_cat_tax
         AND EXPENSE_CODE     = g_exp_code_mpf;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_rate_4 := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '025', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Expense Category TAX',
                 'Expense Code MPF',
                 'Error selecting MPF rate from EXPENSE',
                 NULL
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_hmf_ind := NULL;
      c_rate_5  := 0;

      SELECT HMF_IND INTO c_hmf_ind
        FROM PLCI3225
       WHERE PLACE_CODE = c_entry_port;

      IF c_hmf_ind = 'Y' THEN
          BEGIN
            SELECT NVL(RATE_PER_UNIT_1 * c_customs_val_1, 0)
              INTO c_rate_5

              FROM EXPENSE

             WHERE EXPENSE_CATEGORY = 'TAX'
               AND EXPENSE_CODE     = 'HMF';

            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                c_rate_5 := 0;

              WHEN OTHERS THEN
                p_load_status := 'ERROR';

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '026', 'SELECT', USER,
                       'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Expense Category TAX',
                       'Expense Code HMF',
                       'Error selecting HMF rate from EXPENSE',
                       NULL
                      );
                RAISE REGULAR_ITEM_ERROR;
          END;
      END IF;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_rate_5 := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '027', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Place Code ' || c_entry_port,
                 'Error selecting Harbor Maintenance Fee Indicator'
                    || 'from PLCI3225',
                 NULL, NULL
                );
          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_customs_val_6 := 0;
      c_rate_6 := 0;
      c_link_tbl_name := 'ITEM - ' || c_item;

      SELECT TO_NUMBER(TEXT) INTO c_rate_6

        FROM SPECCOND

       WHERE OWNER         = p_owner
         AND LINK_TBL_NAME = c_link_tbl_name
         AND LINK_KEY_VAL  = c_item
         AND DOC_MSG_CODE IN (g_doc_msg_code_cvd, g_doc_msg_code_adr);

      c_customs_val_6 := c_customs_val_1;
      c_rate_6        := c_rate_6 * c_customs_val_6;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_rate_6 := 0;

          /* Calculate MMV Adjustment Amount excluding MMVFOC */
          IF c_ship_free_ind = 'Y' THEN
              BEGIN
                c_link_tbl_name  := 'INVOICE - ' || c_invoice_no || ' + ' || c_seller;

                c_link_key_val_1 :=  c_invoice_no || ' + ' || c_seller ||
                                     ' + ' || c_bill_of_lading_no || ' + ' ||
                                     c_item || ' + ' || c_invoice_line_no;

                SELECT SUM( NVL( ADJUST_AMOUNT, 0 ) ) INTO c_customs_val_6

                  FROM ADJUST_DTL

                 WHERE OWNER               = p_owner
                   AND LINK_TBL_NAME       = c_link_tbl_name
                   AND LINK_KEY_VAL        = c_link_key_val_1
                   AND EXPENSE_CODE       != g_exp_code_mmvfoc
                   AND EXPENSE_CODE     LIKE g_exp_code_mmvprct
                   AND EXPENSE_CATEGORY    = g_exp_cat_gds;


                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    c_rate_6 := 0;

                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '028', 'SELECT', USER,
                           'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'Invoice ' || c_invoice_no ||' Seller ' || c_seller,
                           'B/L ' || c_bill_of_lading_no || ' Item ' || c_item,
                           'Invoice Line No ' || c_invoice_line_no,
                           'Error select MMV Adjustment Booster off Invoice '
                                || 'Detail'
                          );
                    RAISE REGULAR_ITEM_ERROR;
              END;

          ELSE
              /* Calculate MMV Adjustment Amount including MMVFOC */
              BEGIN
                c_link_tbl_name  := 'INVOICE - ' || c_invoice_no ||' + '|| c_seller;

                c_link_key_val_1 :=  c_invoice_no ||' + '|| c_seller
                                     ||' + '|| c_bill_of_lading_no
                                     ||' + '|| c_item ||' + '|| c_invoice_line_no;

                SELECT SUM( NVL( ADJUST_AMOUNT, 0) ) INTO c_customs_val_6
                  FROM ADJUST_DTL
                 WHERE OWNER               = p_owner
                   AND LINK_TBL_NAME       = c_link_tbl_name
                   AND LINK_KEY_VAL        = c_link_key_val_1
                   AND EXPENSE_CODE     LIKE g_exp_code_mmvprct
                   AND EXPENSE_CATEGORY    = g_exp_cat_gds;

                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    c_rate_6 := 0;

                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '029', 'SELECT', USER,
                           'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'Invoice ' || c_invoice_no ||' Seller ' || c_seller,
                           'B/L ' || c_bill_of_lading_no || ' Item ' || c_item,
                           'Invoice Line No ' || c_invoice_line_no,
                           'Error select MMV Adjustment Booster off Invoice '
                                || 'Detail'
                          );
                    RAISE REGULAR_ITEM_ERROR;
              END;

          END IF;

        WHEN TOO_MANY_ROWS THEN
          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '030', 'SELECT', USER, 'CW',
                 TO_CHAR(SQLCODE), 'INF', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no || ' Item ' || c_item,
                 'Docs/Cond Booster entries exist for both '
                    || 'Countervailing and Anti-Dumping',
                 'Only CVD or ADR Docs/Cond Booster is expected off Item '
                     || 'Header'
                );

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '031', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no || ' Item ' || c_item,
                 'Error selecting Docs/Cond Booster entries for '
                   || 'Countervailing and Anti-Dumping',
                 'Off Item Header'
                );
          RAISE REGULAR_ITEM_ERROR;
    END;


/* ************************************************************************* */


    BEGIN
      c_customs_val_7 := 0;
      c_rate_7 := 0;
      c_link_tbl_name := 'CONTRACT - '|| c_inv_contract_no;

      SELECT SUM(OTHER_COSTS_AMT) INTO c_customs_val_7
        FROM COSTING_DTL
       WHERE OWNER         = p_owner
         AND LINK_TBL_NAME = c_link_tbl_name
         AND LINK_KEY_VAL  = (SELECT CONTRACT_NO || ' + ' || ORDER_NO || ' + ' ||
                                     ITEM || ' + ' || LINE_NO
                                FROM CONTRACT_DTL
                               WHERE OWNER       = p_owner
                                 AND CONTRACT_NO = c_inv_contract_no
                                 AND ORDER_NO    = c_inv_contract_no
                                 AND ITEM        = c_item
                                 AND PRICE       = c_inv_price
                                 AND ROWNUM      = 1
                              )
       AND EXPENSE_CALC_IND != 'H'
       AND EXPENSE_CATEGORY  = 'ROY'
       AND ADD_TO_DUTY_BASE IN ('I', 'O');

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_rate_7 := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '032', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Invoice '|| c_invoice_no || ' Item ' || c_item,
                 'Contract '|| c_inv_contract_no,
                 'B/L '|| c_bill_of_lading_no,
                 'Error select Royalty from COSTING_DTL'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_supplier        := NULL;
      c_item_gsp_status := NULL;
      c_edi_ind         := NULL;
      c_item_gsp_cnt    := 0;
      c_mc_hdr_seller   := NULL;

      BEGIN
        SELECT I.SUPPLIER, NVL(COUNT(UNIQUE NVL(I.STATUS, ' ')), 0)
          INTO c_supplier, c_item_gsp_cnt
          FROM ITEM_DTL I, CONTRACT_HDR C
         WHERE I.OWNER       = p_owner
           AND I.ITEM        = c_item
           AND C.OWNER       = I.OWNER
           AND C.CONTRACT_NO = c_inv_contract_no
           AND I.SUPPLIER    = C.SELLER
         GROUP BY I.SUPPLIER;

        IF (c_item_gsp_cnt > 1) THEN
             p_load_status := SET_LOAD_STATUS('WARN');

             p3 := shrmsglog_pkg.snd_log_msg
                   ('T_SP_RKP4290', p_entry_no, '033', 'IF', USER, 'CW',
                    '1', 'INF', NULL,
                    'Warning - B/L '|| c_bill_of_lading_no,
                    'Invoice '|| c_invoice_no,
                    'Item ' || c_item,
                    ' Multiple GSP statuses exist for Supplier ' || c_supplier ||
                     ' at Item Detail level.'
                   );
        END IF;

        c_mc_hdr_seller := c_supplier;

        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            BEGIN
              c_mc_hdr_seller := NULL;

              SELECT SELLER
                INTO c_mc_hdr_seller
                FROM CONTRACT_HDR
               WHERE OWNER       = p_owner
                 AND CONTRACT_NO = c_inv_contract_no;

              EXCEPTION
                WHEN NO_DATA_FOUND THEN NULL;

                WHEN OTHERS THEN
                  p_load_status := 'ERROR';

                  p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290', p_entry_no, '034', 'SELECT', USER, 'CE',
                         TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                         'B/L '|| c_bill_of_lading_no,
                         'Invoice '|| c_invoice_no,
                         'Contract '|| c_inv_contract_no|| ' Item ' || c_item ,
                         'Error selecting Seller from Contract Header'
                        );

                  RAISE REGULAR_ITEM_ERROR;
            END;

          WHEN OTHERS THEN
            p_load_status := 'ERROR';

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '035', 'SELECT', USER, 'CE',
                   TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                   'B/L '|| c_bill_of_lading_no,
                   'Invoice '|| c_invoice_no,
                   'Item ' || c_item,
                   'Error counting unique GSP Status from Item Detail'
                  );

            RAISE REGULAR_ITEM_ERROR;
      END;

      c_supplier        := NULL;

      SELECT I.SUPPLIER, I.STATUS
        INTO c_supplier, c_item_gsp_status

        FROM ITEM_DTL I, CONTRACT_HDR C

       WHERE I.OWNER       = p_owner
         AND I.ITEM        = c_item
         AND C.OWNER       = I.OWNER
         AND C.CONTRACT_NO = c_inv_contract_no
         AND I.SUPPLIER    = C.SELLER
         AND ROWNUM        = 1;

      IF c_item_gsp_status = 'Y' THEN
          BEGIN
            c_initial_date_1 := NULL;
            c_initial_date_2 := NULL;
            c_link_tbl_name  := 'PARTY - AAFW';

            SELECT NVL(A.INITIAL_DATE, SYSDATE + 1), B.INITIAL_DATE
              INTO c_initial_date_1, c_initial_date_2

              FROM EVENTS_DTL A, EVENTS_DTL B

             WHERE A.LINK_TBL_NAME = c_link_tbl_name
               AND A.LINK_KEY_VAL  = 'AAFW'
               AND A.OWNER         = B.OWNER
               AND A.EVENT_ID      = B.EVENT_ID
               AND A.LINK_TBL_NAME = B.LINK_TBL_NAME
               AND A.LINK_KEY_VAL  = B.LINK_KEY_VAL
               AND A.EVENT_CODE    = 'GEX'
               AND B.EVENT_CODE    = 'GRN';

            IF c_initial_date_2 > c_initial_date_1 AND
               c_initial_date_2 < SYSDATE THEN
                c_edi_ind := c_item_gsp_status;

            ELSIF c_initial_date_1 > SYSDATE AND
                  c_initial_date_2 < SYSDATE THEN
                   c_edi_ind := c_item_gsp_status;

            ELSE
                c_edi_ind := 'A';
            END IF;


            EXCEPTION
              WHEN NO_DATA_FOUND THEN
               c_edi_ind     := NULL;
               p_load_status := SET_LOAD_STATUS('FALSE');

               p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290', p_entry_no, '036', 'SELECT', USER,
                      'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                      'Link Tbl Name is PARTY - AAFW',
                      'Link Key Val is  AAFW',
                      'Item ' || c_item,
                      'GSP date range not found in Events booster off ' ||
                           'Party ID AAFW in Party Table'
                     );

              WHEN TOO_MANY_ROWS THEN
                c_edi_ind     := NULL;
                p_load_status := SET_LOAD_STATUS('FALSE');

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '037', 'SELECT', USER,
                       'CF', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Link Tbl Name is PARTY - AAFW',
                       'Link Key Val is AAFW', 'ITEM - ' || c_item,
                       'Multiple GSP date ranges in Events Booster off ' ||
                          'Party ID AAFW in Party Table'
                      );


              WHEN OTHERS THEN
                p_load_status := 'ERROR';

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '038', 'SELECT', USER,
                       'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Link Tbl Name is PARTY - AAFW',
                       'Link Key Val is AAFW', 'ITEM - ' || c_item,
                       'Error select GSP date ranges in Events Booster off ' ||
                          'Party ID AAFW in PARTY'
                      );

                RAISE REGULAR_ITEM_ERROR;
          END;

      ELSIF (c_item_gsp_status != 'N') THEN
              p_load_status := SET_LOAD_STATUS('FALSE');

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '039', 'IF', USER, 'CF',
                     TO_CHAR(SQLCODE), 'ERR', NULL,
                     'B/L '|| c_bill_of_lading_no,
                     'Invoice '|| c_invoice_no,
                     'Item ' || c_item,
                     'Invalid GSP on Item Detail for supplier ' || c_supplier ||
                           '.  GSP must be Y or N.'
                    );

      END IF;  /* IF c_item_gsp_status = 'Y' */


      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '040', 'SELECT', USER, 'CF',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item || ' Contract ' || c_inv_contract_no,
                 'Supplier ' || c_mc_hdr_seller ||
                      ' not found at Item Detail, GSP status unknown.'
                );

        WHEN TOO_MANY_ROWS THEN
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '041', 'SELECT', USER, 'CF',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item,
                 'Multiple Item Detail records for Status (GSP)'
                );

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '042', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item || ', Supplier ' || c_mc_hdr_seller,
                 'Error selecting GSP Status from Item Detail'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;


    BEGIN
      c_other_costs_amt := 0;
      c_item_duty_amt   := 0;
      c_unit_duty       := 0;
      c_link_tbl_name   := 'CONTRACT - '|| c_inv_contract_no;

      SELECT NVL(SUM(NVL(OTHER_COSTS_AMT, 0)), 0)

        INTO c_other_costs_amt

        FROM COSTING_DTL

       WHERE OWNER         = p_owner
         AND LINK_TBL_NAME = c_link_tbl_name
         AND LINK_KEY_VAL  = (SELECT CONTRACT_NO || ' + ' || ORDER_NO || ' + ' ||
                                     ITEM || ' + ' || LINE_NO
                                FROM CONTRACT_DTL
                               WHERE OWNER       = p_owner
                                 AND CONTRACT_NO = c_inv_contract_no
                                 AND ORDER_NO    = c_inv_contract_no
                                 AND ITEM        = c_item
                                 AND PRICE       = c_inv_price
                                 AND ROWNUM      = 1
                              )

         AND (   (EXPENSE_CODE = 'DUTY' AND EXPENSE_CATEGORY = 'TAX') 
              OR (expense_calc_ind = 'H'));


      c_item_duty_amt := c_other_costs_amt * c_qty_entered;
      c_unit_duty     := c_other_costs_amt + c_rate_4 + c_rate_5;


      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_item_duty_amt := 0;
          c_unit_duty     := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '043', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item || ' Contract ' ||c_inv_contract_no,
                 'Error selecting Item Duty Amt from Costing Detail'
                );
          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_qty_avail_db  := 0;
      c_link_tbl_name := 'SHIPPING - '|| LTRIM(RTRIM(c_bill_of_lading_no));

      SELECT NVL(SUM(NVL(PACK_TOTAL, 0)), 0) INTO c_qty_avail_db

        FROM MATRIX_DTL
       WHERE OWNER = p_owner
         AND LINK_TBL_NAME = c_link_tbl_name
         AND LINK_KEY_VAL || TRUNC(ROW_NO) IN
             (SELECT LTRIM(RTRIM(c_bill_of_lading_no)) ||' + '||
                      LTRIM(RTRIM(EQUIP_ID)) || REF_NO
                FROM SHIPPING_DTL
               WHERE OWNER              = p_owner
                 AND BILL_OF_LADING_NO  = LTRIM(RTRIM(c_bill_of_lading_no))
                 AND CONTRACT_NO        = LTRIM(RTRIM(c_ship_contract_no))
                 AND INVOICE_NO         = LTRIM(RTRIM(c_invoice_no))
                 AND ITEM               = LTRIM(RTRIM(c_item))
                 AND QTY_AVAIL_DB       = c_inv_price
                 AND NVL(EDI_IND, g_edi_ind_x) != g_edi_ind_y
             )

         AND MATRIX_ID = 'CTN'
         AND (RTRIM(ROW_NAME) IS NULL OR RTRIM(ROW_NAME) = 'CTN')
         AND ROW_CODE  = LTRIM(RTRIM(c_item));

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_qty_avail_db := 0;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '044', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item || ' Contract ' ||c_inv_contract_no,
                 'Error select total cartons in Matrix off Shipping Dtl'
                );
          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    BEGIN
      c_hts_no := NULL;

      SELECT LTRIM(RTRIM(HTS_NO)) INTO c_hts_no
        FROM ITEM_HDR
       WHERE OWNER = p_owner
         AND ITEM = LTRIM(RTRIM(c_item));

      IF (c_hts_no IS NULL OR c_hts_no = '')  THEN
          p_load_status := SET_LOAD_STATUS('WARN');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '045', 'IF', USER, 'CW',
                 '1', 'INF', NULL,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item,
                 'HTS# at Item Header is blank.  Update Item Header for item HTS#'
                );
      END IF;

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          c_hts_no := NULL;
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '046', 'SELECT', USER, 'CF',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item,
                 'Item is not on Item Header'
                );

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '047', 'SELECT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'Item ' || c_item,
                 'Error selecting Item HTS number from Item Header'
                );

          RAISE REGULAR_ITEM_ERROR;
    END;

/* ************************************************************************* */

    c_med_check := 'FALSE';

    T_SP_RKP4290_OGA('ITEM', p_owner, p_entry_no, c_bill_of_lading_no,
                     c_import_date, c_export_country, c_item,
                     c_hts_no, c_hts_line_no, nonchargeable_sample,
                     c_origin_ctry_sav, c_med_check, c_load_status);

    p_load_status := SET_LOAD_STATUS(c_load_status);

    IF p_load_status = 'ERROR' THEN
        RAISE REGULAR_ITEM_ERROR;
    END IF;

/* ************************************************************************* */

    IF c_hts_no = 'MULTI' THEN

        IF c_ship_free_ind = 'Y' THEN
            c_inv_price := c_free_price;
        END IF;

    /* Process component (pre-class) attached to Item on Contract detail */
        T_SP_RKP4290_HTSBOOST('PRECLASS', p_owner, NULL, p_entry_no, c_invoice_no_wk,
                              c_invoice_no, c_item, c_line_no,
                              c_item_desc, c_ref_no, c_qty_entered,
                              c_qty_um, c_item_duty_amt, c_unit_duty,
                              c_origin_country, c_rate_1, c_customs_val_1,
                              c_rate_2, c_customs_val_2, c_rate_3,
                              c_customs_val_3, c_rate_4, c_rate_5,
                              c_rate_6, c_customs_val_6, c_rate_7,
                              c_customs_val_7 * c_qty_entered,
                              c_edi_ind, c_qty_avail_db,
                              c_import_date, c_item_gsp_status,
                              c_inv_price, c_med_check, c_bill_of_lading_no,
                              nonchargeable_sample, c_origin_ctry_sav,
                              c_load_status, c_orig_vnd_price, c_discount_rate);

        p_load_status := SET_LOAD_STATUS(c_load_status);

        IF p_load_status = 'ERROR' THEN
            RAISE REGULAR_ITEM_ERROR;
        END IF;

    ELSE
        BEGIN
          IF c_ship_free_ind = 'Y' THEN
              c_inv_price := c_free_price;
          END IF;

          /* *******************************************************************
           * Kavin 2/21/2013 Item Duty Amt will now be stored in both 
           * ITEM_DUTY_AMT and HTS_DUTY_AMT.  The Item Duty Amount is the
           * Quoted Duty or Estimated Duty at the time the entry is created.
           * After entry is cleared by U.S. Customs, program Rkp4296 will
           * update the ITEM_DUTY_AMT with the "real" duty amt. Since
           * HTS_DUTY_AMT contains Quoted Duty, Rockport Impromptu will be able
           * to report difference between estimated duty and real duty at item 
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
                   LAST_UPDATE, LAST_ACTIVITY, P_LINK_KEY_VAL, HTS_DUTY_AMT
                  )

            VALUES(p_owner, p_entry_no, c_invoice_no_wk, c_item, c_line_no,
                   c_item_desc, c_ref_no, c_qty_entered, c_qty_um,
                   c_item_duty_amt, c_unit_duty, c_origin_country, c_hts_no,
                   'E', c_adjusted_inv_price, c_customs_val_1, c_rate_2, c_customs_val_2,
                   c_rate_3, c_customs_val_3, c_rate_4, c_customs_val_4,
                   c_rate_5, c_customs_val_5, c_rate_6, c_customs_val_6,
                   c_rate_7, c_customs_val_7 * c_qty_entered,
                   c_edi_ind, c_qty_avail_db, 'RKP4290', SYSDATE, 'A',
                   p_entry_no || ' + ' || c_invoice_no_wk || ' + ' || c_item || ' + ' ||
                       TO_CHAR(c_line_no, 'FM99999.09'),
                   c_item_duty_amt
                  );

          EXCEPTION
            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '048', 'INSERT', USER,
                     'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'B/L ' || c_bill_of_lading_no,
                     'Invoice No ' || LTRIM(RTRIM(c_invoice_no)),
                     'Item ' || LTRIM(RTRIM(c_item)) || ' HTS# ' ||
                            c_hts_no,
                     'Error adding Finished Good to Entry Detail, '
                            || 'Entry Dtl Line No ' || c_line_no
                    );

             RAISE REGULAR_ITEM_ERROR;
        END;

        BEGIN
          IF (c_discount_price != 0) THEN
              T_SP_RKP4290_REFITEM(p_entry_no,
                                   p_owner,
                                   'OVC',
                                   c_invoice_no_wk,
                                   NULL,
                                   c_item,
                                   c_line_no,
                                   NULL,
                                   NULL,
                                   NULL,
                                   c_orig_vnd_price,
                                   c_load_status );

             p_load_status := SET_LOAD_STATUS(c_load_status);

             IF (c_load_status = 'ERROR') THEN
                 RAISE REGULAR_ITEM_ERROR;
             END IF;
          END IF;

        END;


    END IF;

    IF c_edi_ind = 'A' THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '049', 'IF', USER, 'CW',
               '1', 'INF', NULL,
               'B/L '|| c_bill_of_lading_no, 'Invoice '|| c_invoice_no,
               'Item ' || c_item, 'GSP has expired.'
              );

        c_edi_ind := NULL;
    END IF;

    c_line_no := c_line_no + 1;


  END LOOP L_3;


  BEGIN
    CLOSE C_INVOICE_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '050', 'CLOSE', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || c_bill_of_lading_no,
               'Contract ' || c_ship_contract_no,
               'Seller ' || c_seller || ' Item ' || c_item,
               'Error Closing Invoice Detail cursor for Invoice '
                    || c_invoice_no
               );

        RAISE REGULAR_ITEM_ERROR;
  END;


  IF (c_invoice_dtl_count = 0) THEN
      c_inv_dtl_count := 0;

      BEGIN
        SELECT COUNT(1)

          INTO c_inv_dtl_count

          FROM INVOICE_HDR IH, INVOICE_DTL ID

         WHERE IH.OWNER                 = p_owner
           AND IH.INVOICE_NO            = RTRIM(c_invoice_no)
           AND IH.SELLER               != g_owner_aafw
           AND IH.INV_REF_NO            = RTRIM(c_bill_of_lading_no)

           AND ID.OWNER                 = IH.OWNER
           AND ID.SELLER                = IH.SELLER
           AND ID.INVOICE_NO            = IH.INVOICE_NO
           AND ID.ITEM                  = RTRIM(c_item)
           AND ID.ORDER_NO              = IH.INV_REF_NO
           AND ID.CONTRACT_NO           = RTRIM(c_ship_contract_no);

        EXCEPTION
          WHEN OTHERS THEN
            p_load_status := 'ERROR';

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '051', 'SELECT', USER,
                   'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                   'B/L ' || c_bill_of_lading_no,
                   'Contract ' || c_ship_contract_no,
                   'Seller ' || c_seller || ' Item ' || c_item,
                   'Error counting Invoice Details for Invoice '
                        || c_invoice_no
                   );

            RAISE REGULAR_ITEM_ERROR;
      END;

      IF (c_inv_dtl_count > 0) THEN
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '052', 'IF', USER,
                 'CF', TO_CHAR(SQLCODE), 'ERR', NULL,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item || ', Invoice ' || c_invoice_no,
                 'Vendor Invoice information was not found due to price mismatch'
                  );
      else
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '053', 'IF', USER,
                 'CF', TO_CHAR(SQLCODE), 'ERR', NULL,
                 'B/L ' || c_bill_of_lading_no,
                 'Contract ' || c_ship_contract_no,
                 'Item ' || c_item || ', Invoice ' || c_invoice_no,
                 'Vendor Invoice information was not found'
                );

      END IF;

  END IF;


  EXCEPTION
    WHEN REGULAR_ITEM_ERROR THEN
      IF C_INVOICE_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_INVOICE_DTL;

            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

      RAISE PROCEDURE_ABORT;
END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE REGULAR_ITEMS
** ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL STORED PROCEDURE ATTACH_ENTRY_TEMPLATE
** ************************************************************************* */

PROCEDURE ATTACH_ENTRY_TEMPLATE IS

/* ************************************************************************* **
** local variables declaration.                                              **
** ************************************************************************* */

    l_eth_template_name      EVENTS_TEMPL_HDR.TEMPLATE_NAME%TYPE;

    l_etd_template_name      EVENTS_TEMPL_DTL.TEMPLATE_NAME%TYPE;
    l_etd_event_code         EVENTS_TEMPL_DTL.EVENT_CODE%TYPE;
    l_etd_event_group        EVENTS_TEMPL_DTL.EVENT_GROUP%TYPE;
    l_etd_text               EVENTS_TEMPL_DTL.TEXT%TYPE;
    l_etd_initial_date       EVENTS_TEMPL_DTL.INITIAL_DATE%TYPE;
    l_etd_trigger_event_code EVENTS_TEMPL_DTL.TRIGGER_EVENT_CODE%TYPE;
    l_etd_trigger_add_days   EVENTS_TEMPL_DTL.TRIGGER_ADD_DAYS%TYPE;
    l_etd_value              EVENTS_TEMPL_DTL.VALUE%TYPE;

    c_event_dtl_count        NUMBER;

    SKIP_ENTRY_TEMPLATE      EXCEPTION;
    ENTRY_TEMPLATE_ERROR     EXCEPTION;


BEGIN
  BEGIN
    SELECT TEMPLATE_NAME
      INTO l_eth_template_name
      FROM EVENTS_TEMPL_HDR

     WHERE OWNER         = g_owner_rockblocks
       AND TEMPLATE_NAME = g_fl_entry;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '054', 'SELECT', USER,
               'CW', TO_CHAR(SQLCODE), 'INF', SQLERRM,
               'Owner ' || g_owner_rockblocks,
               'Template Name  ' || g_fl_entry,
               'Template not found in Events Template Header',
               NULL
              );

        RAISE SKIP_ENTRY_TEMPLATE;

      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '055', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Owner ' || g_owner_rockblocks,
               'Template Name  ' || g_fl_entry,
               'Error Selecting Template from Events Template Header',
               NULL
              );

        RAISE ENTRY_TEMPLATE_ERROR;
  END;


   /* Insert FL-ENTRY template hdr into events_hdr table */
  BEGIN
    INSERT INTO EVENTS_HDR
            (
             OWNER,       LINK_TBL_NAME, LINK_KEY_VAL, EVENT_ID,
             TEMPLATE_NO, PARAM1,        PARAM2,       PARAM3,
             PARAM4,      PARAM5,        PARAM6,       EDI_IND,
             LAST_USER,   LAST_UPDATE,   LAST_ACTIVITY
            )

     VALUES (
             p_owner, 'ENTRY - ' || p_entry_no, p_entry_no,
             l_eth_template_name, l_eth_template_name, NULL,
             NULL, NULL, NULL, NULL, NULL, NULL,
             'RKP4290', SYSDATE, 'A'
            );

    EXCEPTION
      WHEN DUP_VAL_ON_INDEX THEN
        NULL;

      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '056', 'INSERT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Ltn ENTRY - ' || p_entry_no,
               'Lkv ' || p_entry_no,
               'Event Id ' || g_fl_entry,
               'Error in creating Event Booster off Entry Header'
              );

        RAISE ENTRY_TEMPLATE_ERROR;
  END;


  BEGIN
    INSERT INTO EVENTS_DTL
             (
              OWNER, LINK_TBL_NAME,
              LINK_KEY_VAL, EVENT_ID,
              EVENT_CODE, EVENT_GROUP,
              TEXT, INITIAL_DATE,
              RESPONSE_DATE, COMPLETE_DATE,
              TRIGGER_EVENT_CODE, TRIGGER_ADD_DAYS,
              PCT_COST_IND, RATE_PER_UNIT,
              UNIT_PRC_BASIS, P_LINK_KEY_VAL,
              EDI_IND, LAST_USER,
              LAST_UPDATE, LAST_ACTIVITY
             )

      SELECT p_owner,    'ENTRY - ' || p_entry_no,
             p_entry_no, TEMPLATE_NAME,
             EVENT_CODE, EVENT_GROUP,
             TEXT,       INITIAL_DATE,
             NULL,       NULL,
             TRIGGER_EVENT_CODE, TRIGGER_ADD_DAYS,
             NULL,       VALUE,
             NULL,       NULL,
             NULL,       'RKP4290',
             SYSDATE,    'A'

        FROM EVENTS_TEMPL_DTL
       WHERE OWNER         = g_owner_rockblocks
         AND TEMPLATE_NAME = g_fl_entry;

      EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
          RAISE SKIP_ENTRY_TEMPLATE;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '057', 'INSERT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error updating ' || g_fl_entry || ' Event Booster off Entry Header',
                 NULL, NULL, NULL
                );

          RAISE ENTRY_TEMPLATE_ERROR;
  END;


  BEGIN
    c_event_dtl_count := 0;
    c_link_tbl_name   := 'ENTRY - ' || p_entry_no;

    SELECT COUNT(1) INTO c_event_dtl_count
      FROM EVENTS_DTL
     WHERE OWNER         = p_owner
       AND LINK_TBL_NAME = c_link_tbl_name
       AND LINK_KEY_VAL  = p_entry_no
       AND EVENT_ID      = g_fl_entry;

    IF c_event_dtl_count = 0 THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '058', 'IF', USER,
               'CW', '1', 'INF', NULL,
               'Ltn ENTRY - ' || p_entry_no,
               'Lkv ' || p_entry_no,
               'No detail created for ' || g_fl_entry || ' Event Booster off Entry Header',
               NULL
              );
    END IF;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '059', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Ltn ENTRY - ' || p_entry_no,
               'Lkv ' || p_entry_no,
               'Error selecting detail from ' || g_fl_entry || ' Event Booster',
               'off Entry Header'
              );

        RAISE ENTRY_TEMPLATE_ERROR;
  END;


  EXCEPTION
    WHEN SKIP_ENTRY_TEMPLATE THEN
      NULL;

    WHEN ENTRY_TEMPLATE_ERROR THEN
      RAISE PROCEDURE_ABORT;
END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE ATTACH_ENTRY_TEMPLATE
** ************************************************************************* */



/* ************************************************************************* **
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290               **
** ************************************************************************* */

BEGIN
  p_load_status         := 'TRUE';
  c_line_no             := 1.0;
  c_notes_count         := 0;
  c_temp_invoice_no     := 'RKP4290_FIRST_FETCH';
  c_ship_invoice_no_tmp := 'RKP4290_FIRST_FETCH';
  c_manipulator         := 10;
  c_load_status         := 'TRUE';

  BEGIN
    SELECT TO_DATE(p_cedate,'MMDDYYYY') into c_date FROM DUAL;

    EXCEPTION
      WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '060', 'SELECT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 '' ,
                 'CE Date '||p_cedate|| ' is invalid  ' ,
                 '  ('||SQLERRM||')', NULL
                );

          RAISE PROCEDURE_ABORT;


  END;



  BEGIN
    OPEN C_REFITEM;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '061', 'OPEN', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Ltn like ' || g_refitem_ltn,
               'Ref No ' || p_entry_no,
               'Reference Qlf ' || g_reference_qlf_ety,
               'Error in Opening Ref No Booster cursor'
               );

       RAISE PROCEDURE_ABORT;
  END;


  <<L_1>>
  LOOP
    c_bill_of_lading_no := NULL;

    BEGIN
      FETCH C_REFITEM
       INTO c_bill_of_lading_no;

      c_refitem_count := C_REFITEM%ROWCOUNT;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '062', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'LTN like ' || g_refitem_ltn,
                 'Ref No ' || p_entry_no,
                 'Reference Qlf ' || g_reference_qlf_ety,
                 'Error in retrieving Ref No booster off Shipping Hdr'
                );

          RAISE PROCEDURE_ABORT;
    END;


    EXIT L_1 WHEN C_REFITEM%NOTFOUND;

/* ************************************************************************* */

/* ****************************    BEGIN ROYALTY CHECK     **************************** */
    BEGIN
      OPEN C_ROYALTY_CHECK;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '063', 'OPEN', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error opening ROYALTY CHECK CURSOR',
                 NULL, NULL, NULL
                );

          RAISE PROCEDURE_ABORT;
    END;

/* ************************************************************************* */

    <<ROYALTY_CHECK_LOOP>>
    LOOP
      c_item             := NULL;
      c_expense_code     := NULL;
      c_rec_id           := NULL;
      c_expense_category := NULL;
      c_ship_contract_no := NULL;

      FETCH C_ROYALTY_CHECK
       INTO c_item, c_expense_code, c_rec_id, c_expense_category, c_ship_contract_no;

      EXIT ROYALTY_CHECK_LOOP WHEN C_ROYALTY_CHECK%NOTFOUND;


/* ************************************************************************* */

      p_load_status := SET_LOAD_STATUS('FALSE');

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290', p_entry_no, '064', 'IF', USER, 'CF',
             '1', 'ERR', NULL,
             'Contract No/item '||c_ship_contract_no||'/'||c_item||
                  ' has an invalid royalty expense code' ,
             'B/L '|| c_bill_of_lading_no,
             ' expense code - '|| c_expense_code,
             ' '
             );

/* ************************************************************************* */

    END LOOP ROYALTY_CHECK_LOOP;


    BEGIN
      CLOSE C_ROYALTY_CHECK;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '065', 'OPEN', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error Closing Royalty check cursor',
                 NULL, NULL, NULL
                );
          RAISE PROCEDURE_ABORT;
    END;


/* ***************************       END ROYALTY CHECK   ******************* */


    BEGIN
    IF (p_cedate is not null) THEN
      BEGIN
         UPDATE eventS_dtl
            SET complete_date = TO_DATE(p_cedate,'MMDDYYYY'),
            last_update = sysdate,
            last_user = 'RKP4292VB'
         WHERE
               owner = p_owner
            AND link_tbl_name = 'SHIPPING - '||c_bill_of_lading_no
            AND event_code = 'WR'
            AND (('COU' IN (SELECT distinct movement_type FROM SHIPPING_dtl
                     WHERE OWNER = p_owner AND
                           BILL_OF_LADING_NO = C_BILL_OF_LADING_NO)) or
                 ('HAND' IN (SELECT distinct movement_type FROM SHIPPING_dtl
                     WHERE OWNER = p_owner AND
                           BILL_OF_LADING_NO = C_BILL_OF_LADING_NO)) or
                 ('POST' IN (SELECT distinct movement_type FROM SHIPPING_dtl
                     WHERE OWNER = p_owner AND
                           BILL_OF_LADING_NO = C_BILL_OF_LADING_NO)));

      EXCEPTION
         WHEN OTHERS THEN
            p_load_status := 'ERROR';

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '066', 'SELECT', USER,
                  'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                  '' ,
                  'Error updating WHSE receive date'||c_bill_of_lading_no ,
                  '  ('||SQLERRM||')', NULL
                  );

            RAISE PROCEDURE_ABORT;

      END;
     END IF;

    END;

    c_rate_1         := 0;
    c_rate_2         := 0;
    c_rate_3         := 0;
    c_rate_4         := 0;
    c_rate_5         := 0;
    c_rate_6         := 0;
    c_rate_7         := 0;
    c_customs_val_1  := 0;
    c_customs_val_2  := 0;
    c_customs_val_3  := 0;
    c_customs_val_4  := 0;
    c_customs_val_5  := 0;
    c_customs_val_6  := 0;
    c_customs_val_7  := 0;
    c_import_date    := NULL;
    c_load_point     := NULL;
    c_transport_mode := NULL;
    c_carrier        := NULL;
    c_voyage_number  := NULL;
    c_final_dest     := NULL;

    BEGIN
      SELECT ACT_ARRIVE_DATE, DISCHARGE_POINT, TRANSPORT_MODE, CARRIER,
             VOYAGE_NUMBER, FINAL_DEST

        INTO c_import_date, c_load_point, c_transport_mode, c_carrier,
             c_voyage_number, c_final_dest

        FROM SHIPPING_HDR
       WHERE OWNER             = p_owner
         AND BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no));

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '067', 'SELECT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || c_bill_of_lading_no,
                 'Error selecting B/L from Shipment Header',
                 NULL, NULL
                );

          RAISE PROCEDURE_ABORT;
    END;

/* ************************************************************************* */

    IF (p_entry_port IS NULL) THEN
        IF (RTRIM(c_transport_mode) = 'OCN') THEN
             IF (RTRIM(c_load_point) IN ('2704', '2709')) THEN
                 c_entry_port := '2704';

             ELSIF (RTRIM(c_load_point) IN ('2809', '2811')) THEN
                 c_entry_port := '2809';

             ELSE
                 c_entry_port := RTRIM(c_load_point);
             END IF;

        ELSIF (c_transport_mode = 'AIR') THEN
             BEGIN
               SELECT PLACE_CODE INTO c_entry_port
                 FROM PLACES
                WHERE PLACE_NAME LIKE '%'|| RTRIM(c_final_dest) ||'%'
                  AND ROWNUM = 1;

               EXCEPTION
                 WHEN NO_DATA_FOUND THEN
                   c_entry_port  := c_final_dest;
                   p_load_status := SET_LOAD_STATUS('WARN');

                   p3 := shrmsglog_pkg.snd_log_msg
                         ('T_SP_RKP4290', p_entry_no, '068', 'SELECT',
                          USER, 'CW', TO_CHAR(SQLCODE), 'INF', SQLERRM,
                          'B/L ' || c_bill_of_lading_no,
                          'Cannot find entry port code in Places table for'
                             || ' destination', c_final_dest,
                          'Final destination will be used for entry port'
                         );

                 WHEN OTHERS THEN
                   p_load_status := 'ERROR';

                   p3 := shrmsglog_pkg.snd_log_msg
                         ('T_SP_RKP4290', p_entry_no, '069', 'SELECT', USER,
                          'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                          'B/L ' || c_bill_of_lading_no,
                          'Final Destination ' || c_final_dest,
                          'Error in selecting  entry port for ' ||
                              'Final Destination',
                          NULL
                         );

                   RAISE PROCEDURE_ABORT;
             END;

             c_carrier := SUBSTR(c_voyage_number, 1, 2);
        END IF;
    ELSE
        c_entry_port := p_entry_port;
    END IF;

/* ************************************************************************* */

    BEGIN
      INSERT INTO ENTRY_HDR
              (OWNER, ENTRY_NO, ENTRY_TYPE, ENTRY_DATE, IMPORT_DATE, ENTRY_PORT,
               LOAD_POINT, IMPORTER, CARRIER, PREVIOUSLY_USED, STATUS,
               CUSTOMS_BROKER_ID, LAST_USER, LAST_UPDATE, LAST_ACTIVITY
              )

        VALUES(p_owner, p_entry_no, '01', to_date(P_CEDATE,'mm/dd/yyyy'), c_import_date, c_entry_port,
               c_load_point, '75-218872300', c_carrier, 'N', 'EN', p_cleared,
               'RKP4290', SYSDATE, 'A'
              );

      EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
          NULL;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '070', 'INSERT', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error in creating Entry Header',
                 NULL, NULL, NULL
                );

          RAISE PROCEDURE_ABORT;
    END;

/* ************************************************************************* */
/*
    BEGIN
      c_note_id := 'EXAMSITE';
      c_template_name := c_entry_port;

      WHILE c_notes_count < 2
      LOOP
        IF c_notes_count = 1 THEN
            c_note_id := 'FIRMSCODE';

            IF (c_entry_port IS NULL) THEN
                 c_template_name := p_movement_type || c_load_point
                   || p_trans_id;
            ELSIF (c_entry_port = '5501') THEN
                 c_template_name := p_movement_type || c_entry_port
                   || c_carrier;
            ELSE
                 c_template_name := p_movement_type || c_entry_port
                   || p_trans_id;
            END IF;
        END IF;


    BEGIN
          INSERT INTO NOTES_HDR
                   (OWNER, LINK_TBL_NAME, LINK_KEY_VAL, NOTE_ID,
                    TEMPLATE_NAME, LAST_USER, LAST_UPDATE, LAST_ACTIVITY
                   )

            VALUES (g_owner_aafw, 'ENTRY - ' || p_entry_no, p_entry_no, c_note_id,
                    c_template_name, 'RKP4290', SYSDATE, 'A'
                   );

          EXCEPTION
            WHEN DUP_VAL_ON_INDEX THEN
              NULL;

            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '071', 'SELECT', USER, 'CE',
                     TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'Template Name ' || c_template_name,
                     'Note ID ' || c_note_id,
                     'Error Creating Notes Booster off Entry Header',
                     NULL
                    );

              RAISE PROCEDURE_ABORT;
    END;
 *************************************************************************

        IF (c_transport_mode = 'AIR') THEN
            c_template_name := 'AIR';
        END IF;

        INSERT INTO NOTES_DTL
                 (OWNER, LINK_TBL_NAME, LINK_KEY_VAL, NOTE_ID, LINE_NO,
                  TEXT, LAST_USER, LAST_UPDATE, LAST_ACTIVITY
                 )

          SELECT g_owner_aafw, 'ENTRY - ' || p_entry_no, p_entry_no, c_note_id,
                 LINE_NO, TEXT, 'RKP4290', SYSDATE, 'A'
            FROM NOTES_TEMPL_DTL
           WHERE TEMPLATE_NAME = c_template_name
             AND NOTE_ID       = c_note_id;


    BEGIN
      c_notes_dtl_count := 0;
          c_link_tbl_name   := 'ENTRY - ' || p_entry_no;

          SELECT COUNT(1) INTO c_notes_dtl_count
            FROM NOTES_DTL
           WHERE OWNER         = g_owner_aafw
             AND LINK_TBL_NAME = c_link_tbl_name
             AND LINK_KEY_VAL  = p_entry_no
             AND NOTE_ID       = c_note_id;

          IF c_notes_dtl_count = 0 THEN
              p_load_status := SET_LOAD_STATUS('WARN');

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '072', 'IF', USER,
                     'CW', '1', 'INF', NULL,
                     'Ltn ENTRY - ' || p_entry_no,
                     'Lkv ' || p_entry_no,
                     'Note ID ' || c_note_id,
                     'No detail created for Notes Booster off Entry Header'
                    );
          END IF;

          EXCEPTION
            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '073', 'SELECT', USER, 'CE',
                     TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'Template Name ' || c_template_name,
                     'Note ID ' || c_note_id,
                     'Error counting detail in Notes Booster off Entry Header',
                     NULL
                    );

              RAISE PROCEDURE_ABORT;
        END;

        c_notes_count := c_notes_count + 1;
      END LOOP;


      EXCEPTION
        WHEN DUP_VAL_ON_INDEX THEN
          NULL;

        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '074', 'INSERT', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Template Name ' || c_template_name,
                 'Note ID ' || c_note_id,
                 'Error in creating or updating Notes Booster '
                   || ' off Entry Header',
                 NULL
                );

          RAISE PROCEDURE_ABORT;
    END;
*/
/* ******************************************************************************************** */

/*   This is is the beginning of processing for BOL.  Non-chargeable items are processed first. */
    BEGIN

        BEGIN
          OPEN C_SHIPPING_DTL_SAMPLE_INVOICE;

          EXCEPTION
            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '075', 'OPEN', USER, 'CE',
                     TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'Error opening Shipping Detail cursor for Non-chargeable invoices',
                     'BOL: ' || c_bill_of_lading_no,
                     'EDI Ind: Y',
                     NULL
                    );

              RAISE PROCEDURE_ABORT;
        END;

        <<SAMPLE_INVOICE_LOOP>>
        LOOP

          c_invoice_no       := NULL;
          c_sample_dtl_count := 0;

          BEGIN
            FETCH C_SHIPPING_DTL_SAMPLE_INVOICE
             INTO c_invoice_no;

            EXCEPTION
              WHEN OTHERS THEN
                p_load_status := 'ERROR';

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '076', 'FETCH', USER, 'CE',
                       TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Fetch on C_SHIPPING_DTL_SAMPLE_INVOICE failed',
                       'BOL: ' || c_bill_of_lading_no,
                       'EDI Ind: Y',
                       NULL
                      );

                RAISE PROCEDURE_ABORT;
          END;


          EXIT SAMPLE_INVOICE_LOOP WHEN C_SHIPPING_DTL_SAMPLE_INVOICE%NOTFOUND;

--          c_invoice_no := LTRIM(RTRIM(c_invoice_no));

          c_line_no := 1;

          BEGIN
              BEGIN
                OPEN C_SHIPPING_DTL_SAMPLE;

                EXCEPTION
                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '077', 'OPEN', USER, 'CE',
                           TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'Error opening Shipping Detail cursor for Samples',
                           'BOL: ' || c_bill_of_lading_no,
                           'EDI Ind: Y',
                           'Sample Invoice No: ' || c_invoice_no
                          );

                    RAISE PROCEDURE_ABORT;
              END;

              c_last_item := NULL;

          /* ************************************************************************************ */

              <<SAMPLE_SHIPPING_DTL_LOOP>>
              LOOP
                nonchargeable_sample := 'FALSE';
                c_item               := NULL;
                c_ship_contract_no   := NULL;
                c_invoice_no         := NULL;
                c_origin_country     := NULL;
                c_ship_edi_ind       := NULL;
                c_buyer              := NULL;


                BEGIN
                  FETCH C_SHIPPING_DTL_SAMPLE
                   INTO c_item, c_ship_contract_no, c_invoice_no,
                        c_origin_country, c_ship_edi_ind, c_manufacturer;

                  EXCEPTION
                    WHEN OTHERS THEN
                      p_load_status := 'ERROR';

                      p3 := shrmsglog_pkg.snd_log_msg
                            ('T_SP_RKP4290', p_entry_no, '078', 'FETCH', USER, 'CE',
                             TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                             'Error selecting from Shipping Detail cursor for Samples',
                             'BOL: ' || c_bill_of_lading_no,
                             'EDI Ind: Y',
                             'Sample Invoice No: ' || c_invoice_no
                            );

                      RAISE PROCEDURE_ABORT;
                END;

                c_sample_dtl_count := C_SHIPPING_DTL_SAMPLE%ROWCOUNT;

                EXIT SAMPLE_SHIPPING_DTL_LOOP WHEN C_SHIPPING_DTL_SAMPLE%NOTFOUND;

                c_origin_ctry_sav := LTRIM(RTRIM(c_origin_country));

                BEGIN
                  SELECT UNIQUE DELIVER_TO INTO c_buyer

                    FROM SHIPPING_DTL

                   WHERE OWNER             = LTRIM(RTRIM(p_owner))
                     AND BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
                     AND ITEM              = LTRIM(RTRIM(c_item))
                     AND ROWNUM            = 1;

                  EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                      c_buyer       := 'NO CUSTOMER';
                      p_load_status := SET_LOAD_STATUS('WARN');

                      p3 := shrmsglog_pkg.snd_log_msg
                            ('T_SP_RKP4290', p_entry_no, '079', 'SELECT', USER,
                             'CW', TO_CHAR(SQLCODE), 'INF', SQLERRM, 'B/L '||
                             c_bill_of_lading_no, 'Invoice '|| c_invoice_no,
                             'Buyer (Deliver_to) is not defined on Shipping Dtl',
                             NULL
                            );

                    WHEN OTHERS THEN
                      p_load_status := 'ERROR';

                      p3 := shrmsglog_pkg.snd_log_msg
                            ('T_SP_RKP4290', p_entry_no, '080', 'SELECT', USER,
                             'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                             'B/L '|| c_bill_of_lading_no,
                             'Invoice '|| c_invoice_no,
                             'Error selecting Buyer (Deliver_to) from Shipping Dtl',
                             NULL
                            );
                      RAISE PROCEDURE_ABORT;
                END;

          /* ************************************************************************* */

                nonchargeable_sample := 'TRUE';
          --      c_manufacturer       := 'NCSAMPLE';
          --      c_invoice_no         := 'FOC/'||c_bill_of_lading_no;

          /* ************************************************************************* */

                IF c_last_item != c_item OR c_last_item is NULL THEN
                  T_SP_RKP4290_SAMPLE(p_entry_no, p_owner, c_bill_of_lading_no,
                                        c_import_date, p_entry_port,
                                        c_invoice_no, c_item,
                                        c_item_gsp_status, c_origin_country,
                                        c_manufacturer,
                                        c_edi_ind, c_line_no,
                                        c_med_check, c_load_status);

                  p_load_status := SET_LOAD_STATUS(c_load_status);

                  IF p_load_status = 'ERROR' THEN
                        RAISE PROCEDURE_ABORT;
                  END IF;
                END IF;

                c_last_item := c_item;

          /* ************************************************************************* */

              END LOOP SAMPLE_SHIPPING_DTL_LOOP;


              IF (c_sample_dtl_count = 0) THEN
                  p_load_status := SET_LOAD_STATUS('FALSE');

                  p3 := shrmsglog_pkg.snd_log_msg
                        ('T_SP_RKP4290', p_entry_no, '081', 'IF', USER,
                         'CF', TO_CHAR(SQLCODE), 'ERR', NULL,
                         'B/L ' || c_bill_of_lading_no,
                         'EDI Ind of Y',
                         'Sample Invoice ( ' || c_invoice_no || ' )',
                         'Corresponding Sample Detail information not found'
                        );
              END IF;

              BEGIN
                CLOSE C_SHIPPING_DTL_SAMPLE;

                EXCEPTION
                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '082', 'CLOSE', USER, 'CE',
                           TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'Error Closing Shipping Detail cursor for Samples',
                           'BOL: ' || c_bill_of_lading_no,
                           'EDI Ind: Y',
                           'Sample Invoice No: ' || c_invoice_no
                          );

                    RAISE PROCEDURE_ABORT;
              END;


              /*  Issue 4190 - Set QTY_AVAIL_DB equal to 0.00 */
              BEGIN
                UPDATE SHIPPING_DTL
                   SET QTY_AVAIL_DB      = 0.00
                 WHERE OWNER             = p_owner
                   AND BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
                   AND (QTY_AVAIL_DB     IS NULL OR QTY_AVAIL_DB != 0.00)
                   AND EDI_IND           IN (g_edi_ind_f, g_edi_ind_m, g_edi_ind_s);

                EXCEPTION
                  WHEN NO_DATA_FOUND THEN NULL;

                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '083', 'UPDATE', USER, 'CE',
                           TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'B/L '|| c_bill_of_lading_no,
                           'EDI Ind in (F, M, S)',
                           'Unable to update missing free price (qty_avail_db) at Shipping Dtl.',
                           NULL
                          );
              END;


              /*  Issue 4190 - Set QTY_AVAIL_DB equal to VFC Ref No Value  */
              BEGIN
                UPDATE SHIPPING_DTL SD
                   SET QTY_AVAIL_DB = ( SELECT TO_NUMBER(REF_NO)
                                          FROM REFITEM
                                         WHERE OWNER         = SD.OWNER

                                           AND LINK_TBL_NAME = g_ltn_shipping ||
                                                                 SD.BILL_OF_LADING_NO

                                           AND LINK_KEY_VAL  = SD.BILL_OF_LADING_NO || ' + ' ||
                                                                 SD.EQUIP_ID || ' + ' || SD.ITEM
                                                                 || ' + ' || SD.REF_NO

                                           AND REFERENCE_QLF = g_reference_qlf_vfc
                                           AND REF_NO        IS NOT NULL)

                 WHERE OWNER             = p_owner
                   AND BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
                   AND NVL(EDI_IND, g_edi_ind_x) NOT IN (g_edi_ind_y, g_edi_ind_f, g_edi_ind_m, g_edi_ind_s)
                   AND EXISTS (SELECT REF_NO
                                 FROM REFITEM
                                WHERE OWNER         = SD.OWNER

                                  AND LINK_TBL_NAME = g_ltn_shipping || SD.BILL_OF_LADING_NO

                                  AND LINK_KEY_VAL  = SD.BILL_OF_LADING_NO || ' + ' ||
                                                        SD.EQUIP_ID || ' + ' || SD.ITEM
                                                        || ' + ' || SD.REF_NO

                                  AND REFERENCE_QLF = g_reference_qlf_vfc
                                  AND REF_NO        IS NOT NULL);


                EXCEPTION
                  WHEN NO_DATA_FOUND THEN NULL;

                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290', p_entry_no, '084', 'UPDATE', USER, 'CE',
                           TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                           'B/L '|| c_bill_of_lading_no,
                           'Unable to update missing non-free price (qty_avail_db)' ||
                              ' at Shipping Dtl.',
                           'Using ' || g_reference_qlf_vfc ||
                              ' Ref No Boosters at Shipping Detail level',
                           NULL
                          );
              END;

          END;

--          PROCESS_SAMPLE_INVOICE();

    /* ************************************************************************* */

        END loop SAMPLE_INVOICE_LOOP;

        BEGIN
          CLOSE C_SHIPPING_DTL_SAMPLE_INVOICE;

          EXCEPTION
            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '085', 'OPEN', USER, 'CE',
                     TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'Error Closing Shipping Detail cursor for Non-chargeable invoices',
                     'BOL: ' || c_bill_of_lading_no,
                     'EDI Ind: Y',
                     NULL
                    );

              RAISE PROCEDURE_ABORT;
        END;


    END;

/*     ************************
       ************************ */

    BEGIN
      OPEN C_SHIPPING_DTL_REGULAR;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '086', 'OPEN', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error opening Shipping Detail cursor for Chargeable Items',
                 NULL, NULL, NULL
                );

          RAISE PROCEDURE_ABORT;
    END;

    c_last_item := NULL;

/* ***************chargeable item processing******************************************* */

    <<REGULAR_SHIPPING_DTL_LOOP>>
    LOOP
      c_item             := NULL;
      c_ship_contract_no := NULL;
      c_invoice_no       := NULL;
      c_origin_country   := NULL;
      c_ship_edi_ind     := NULL;
      c_buyer            := NULL;
      c_contract_seller  := NULL;

      FETCH C_SHIPPING_DTL_REGULAR
       INTO c_item,           c_ship_contract_no,  c_invoice_no,
            c_origin_country, c_ship_qty_avail_db, c_ship_edi_ind,
            c_temp,           c_contract_seller;

      EXIT REGULAR_SHIPPING_DTL_LOOP WHEN C_SHIPPING_DTL_REGULAR%NOTFOUND;

      c_origin_ctry_sav := LTRIM(RTRIM(c_origin_country));

      c_ship_invoice_no := c_invoice_no;

      IF c_ship_invoice_no_tmp != c_ship_invoice_no THEN
          BEGIN
            c_seller_count := 0;

            SELECT COUNT(1) into c_seller_count

              FROM INVOICE_HDR

             WHERE OWNER       = p_owner
               AND INVOICE_NO  = LTRIM(RTRIM(c_invoice_no))
               AND SELLER     != g_owner_aafw
               AND INV_REF_NO  = LTRIM(RTRIM(c_bill_of_lading_no));

            EXCEPTION

              WHEN OTHERS THEN
                p_load_status := 'ERROR';

                p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290', p_entry_no, '087', 'SELECT', USER,
                       'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Invoice No ' || c_invoice_no,
                       'B/L ' || c_bill_of_lading_no,
                       'Error counting Sellers on Invoice Header',
                       NULL
                      );

                RAISE PROCEDURE_ABORT;
          END;

          c_ship_invoice_no_tmp := c_ship_invoice_no;

      END IF;

      IF (c_seller_count > 1) THEN
          IF (NVL(LTRIM(RTRIM(c_contract_seller)), ' ') = ' ') THEN
              p_load_status := SET_LOAD_STATUS('FALSE');

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290', p_entry_no, '088', 'IF', USER, 'CF',
                     '1', 'ERR', NULL,
                     'Seller is blank on Contract ' || c_ship_contract_no,
                     'B/L '|| c_bill_of_lading_no,
                     'Invoice '|| c_invoice_no,
                     ''
                    );
          END IF;

          c_ship_invoice_no_wk := LTRIM(RTRIM(c_ship_invoice_no)) || '-' || c_contract_seller;

      ELSE
          c_ship_invoice_no_wk := LTRIM(RTRIM(c_ship_invoice_no));

      END IF;

      c_line_no := GET_MAX_LINE_NO(p_owner, p_entry_no, c_ship_invoice_no_wk);

      BEGIN
        SELECT UNIQUE DELIVER_TO INTO c_buyer

          FROM SHIPPING_DTL

         WHERE OWNER             = LTRIM(RTRIM(p_owner))
           AND BILL_OF_LADING_NO = LTRIM(RTRIM(c_bill_of_lading_no))
           AND ITEM              = LTRIM(RTRIM(c_item))
           AND INVOICE_NO        = LTRIM(RTRIM(c_invoice_no))
           AND QTY_AVAIL_DB      = c_ship_qty_avail_db
           AND ROWNUM            = 1;

        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            c_buyer       := 'NO CUSTOMER';
            p_load_status := SET_LOAD_STATUS('WARN');

            p3 := shrmsglog_pkg.snd_log_msg
                  ('T_SP_RKP4290', p_entry_no, '089', 'SELECT', USER, 'CW',
                   TO_CHAR(SQLCODE), 'INF', SQLERRM,
                   'B/L '|| c_bill_of_lading_no,
                   'Invoice '|| c_invoice_no,
                   'Buyer (Deliver_to) is not defined on SHIPPING_DTL.',
                   NULL
                  );
      END;

/* ************************************************************************* */

      IF (c_ship_contract_no is NULL OR c_ship_contract_no = '') AND
         c_ship_edi_ind = 'N' THEN
          p_load_status := SET_LOAD_STATUS('FALSE');

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '090', 'IF', USER, 'CF',
                 '1', 'ERR', NULL,
                 'Contract No is blank and Shipment tracking detail ' ||
                        'edi_ind is not Y',
                 'B/L '|| c_bill_of_lading_no,
                 'Invoice '|| c_invoice_no,
                 'No contract associated with this item in Shipment ' ||
                        'Tracking dtl'
                );
      END IF;

/* ************************************************************************* */

      REGULAR_ITEMS( );

/* ************************************************************************* */

    END LOOP REGULAR_SHIPPING_DTL_LOOP;


    BEGIN
      CLOSE C_SHIPPING_DTL_REGULAR;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290', p_entry_no, '091', 'OPEN', USER, 'CE',
                 TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error Closing Shipping Detail cursor for Chargeable Items',
                 NULL, NULL, NULL
                );

          RAISE PROCEDURE_ABORT;
    END;


  END LOOP L_1;


  BEGIN
    CLOSE C_REFITEM;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '092', 'OPEN', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Ltn like ' || g_refitem_ltn,
               'Ref No ' || p_entry_no,
               'Reference Qlf ' || g_reference_qlf_ety,
               'Error in Closing Ref No Booster cursor'
               );

       RAISE PROCEDURE_ABORT;
  END;


  IF ( c_refitem_count = 0) THEN
      RAISE NO_REFITEM_ROWS;
  END IF;

/* ************************************************************************* **
**    UPDATE OF ENTRY_HDR, ENTRY_INV AND ENTRY_DTL TABLES                    **
** ************************************************************************* */

  BEGIN
    UPDATE ENTRY_INV EI  SET
        INV_TOTAL_VAL = (SELECT NVL(SUM(NVL(CUSTOMS_VAL_1, 0) ), 0)

                           FROM ENTRY_DTL
                          WHERE OWNER      = p_owner
                            AND ENTRY_NO   = p_entry_no
                            AND INVOICE_NO = EI.INVOICE_NO),

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
                      AND INVOICE_NO = EI.INVOICE_NO)

        WHERE OWNER    = p_owner
          AND ENTRY_NO = p_entry_no;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '093', 'UPDATE', USER, 'CE',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error updating Entry Invoice ',
               NULL, NULL, NULL
              );

        RAISE PROCEDURE_ABORT;
  END;


  BEGIN
    SELECT NVL(MAX_AMOUNT,0), NVL(MIN_AMOUNT, 0)
      INTO c_max_amount, c_min_amount
      FROM EXPENSE
     WHERE EXPENSE_CATEGORY = 'TAX'
       AND EXPENSE_CODE     = 'MPF'
       AND ROWNUM = 1;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_load_status := 'WARN';
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '094', 'SELECT', USER, 'CW',
               TO_CHAR(SQLCODE), 'INF', SQLERRM,
               'Expense Category TAX',
               'Expense Code MPF',
               'Unable to select min and max Merchandise Processing '
                     || 'Fee amounts',
               'Entry not found in Expense table'
              );

      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '095', 'SELECT', USER, 'CE',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Expense Category TAX',
               'Expense Code MPF',
               'Error selecting min and max Merchandise Processing '
                    || 'Fee amounts from EXPENSE',
               NULL
              );

        RAISE PROCEDURE_ABORT;
  END;

  BEGIN
    c_entry_hmf_amt := 0;

    SELECT SUM(RATE_5) INTO c_entry_hmf_amt
      FROM ENTRY_DTL
     WHERE OWNER    = p_owner
       AND ENTRY_NO = p_entry_no;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '096', 'SELECT', USER, 'CE',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error selecting total harbor maintenance fee '
                    || 'from Entry Detail',
               'Error updating Entry Invoice ',
               NULL, NULL
              );
        RAISE PROCEDURE_ABORT;
  END;

  BEGIN
    c_total_mpf := 0;

    SELECT SUM(RATE_4) INTO c_total_mpf
      FROM ENTRY_DTL
     WHERE OWNER    = p_owner
       AND ENTRY_NO = p_entry_no;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '097', 'SELECT', USER, 'CE',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error selecting total merchandise processing fee '
                    || 'from Entry Detail',
               NULL, NULL, NULL
              );

        RAISE PROCEDURE_ABORT;
  END;

  BEGIN
    c_total_duty := 0;

    SELECT NVL(SUM(NVL(ITEM_DUTY_AMT, 0)), 0)

      INTO c_total_duty

      FROM ENTRY_DTL

     WHERE OWNER    = p_owner
       AND ENTRY_NO = p_entry_no
       AND (   (LINE_NO = TRUNC(LINE_NO))
            OR (LINE_NO = TRUNC(LINE_NO) + 0.1));

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := SET_LOAD_STATUS('FALSE');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '098', 'SELECT', USER, 'CF',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error selecting total duty from Entry Detail',
               NULL, NULL, NULL
              );
  END;

  BEGIN
    c_total_value := 0;

    SELECT SUM(TOT_AMT) INTO c_total_value
      FROM ENTRY_INV
     WHERE OWNER    = p_owner
       AND ENTRY_NO = p_entry_no;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := SET_LOAD_STATUS('FALSE');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '099', 'SELECT', USER, 'CF',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error selecting total value from Entry Invoice',
               NULL, NULL, NULL
              );
  END;

  IF c_transport_mode != 'OCN' THEN
      c_entry_hmf_amt := 0;
  END IF;

  IF c_transport_mode = 'OCN' THEN
      IF c_total_mpf > c_max_amount THEN
          c_total_mpf := c_max_amount;
      END IF;
  END IF;

  IF c_transport_mode = 'AIR' THEN
      IF c_total_value < 2000 THEN
          c_total_mpf := 2;
          c_entry_hmf_amt := 0;
      ELSE
          IF c_total_mpf < c_min_amount THEN
              c_total_mpf := c_min_amount;
          ELSIF c_total_mpf > c_max_amount THEN
              c_total_mpf := c_max_amount;
          END IF;
      END IF;
  END IF;

  c_total_other      := c_total_mpf + c_entry_hmf_amt;
  c_tot_duty_tax_fee := c_total_mpf + c_entry_hmf_amt + c_total_duty;

  BEGIN
    UPDATE ENTRY_HDR
       SET TOTAL_DUTY       = c_total_duty,
           ENTRY_HMF_AMT    = c_entry_hmf_amt,
           TOTAL_MPF        = c_total_mpf,
           TOTAL_VALUE      = c_total_value,
           TOTAL_OTHER      = c_total_other,
           TOT_DUTY_TAX_FEE = c_tot_duty_tax_fee

     WHERE OWNER    = p_owner
       AND ENTRY_NO = p_entry_no;


    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '100', 'UPDATE', USER, 'CE',
               TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error updating Entry Header.',
               NULL, NULL, NULL
              );

    RAISE PROCEDURE_ABORT;
  END;


  ATTACH_ENTRY_TEMPLATE( );


  BEGIN
    c_entry_dtl_count := 0;

    SELECT COUNT(1) into c_entry_dtl_count
      FROM ENTRY_DTL
     WHERE OWNER       = p_owner
       AND ENTRY_NO    = LTRIM(RTRIM(p_entry_no));


    EXCEPTION

      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290', p_entry_no, '101', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error counting Entry detail rows ',
               NULL,
               NULL,
               NULL
              );

        RAISE PROCEDURE_ABORT;
  END;



  IF (c_entry_dtl_count > 0) THEN
      T_SP_RKP4290_MXPK(p_entry_no, p_owner, c_load_status);

      p_load_status := SET_LOAD_STATUS(c_load_status);

      IF p_load_status = 'ERROR' THEN
          RAISE PROCEDURE_ABORT;
      END IF;

  ELSE
      p_load_status := SET_LOAD_STATUS('FALSE');

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290', p_entry_no, '102', 'CLUMPING', USER, 'CE',
             TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'No Entry Detail records were created for this entry',
             NULL, NULL, NULL
            );
  END IF;


  EXCEPTION
    WHEN PROCEDURE_ABORT THEN
      IF C_REFITEM%ISOPEN THEN
          BEGIN
            CLOSE C_REFITEM;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_SHIPPING_DTL_REGULAR%ISOPEN THEN
          BEGIN
            CLOSE C_SHIPPING_DTL_REGULAR;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_SHIPPING_DTL_SAMPLE%ISOPEN THEN
          BEGIN
            CLOSE C_SHIPPING_DTL_SAMPLE;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_INVOICE_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_INVOICE_DTL;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

    WHEN NO_REFITEM_ROWS THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290', p_entry_no, '103', 'CLUMPING', USER, 'CE',
             TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'No Bills of Lading selected for this entry ',
             NULL, NULL, NULL
            );


    WHEN OTHERS THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290', p_entry_no, '104', 'CLUMPING', USER, 'CE',
             TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'Unexpected SQL error in Stored Procedure',
             NULL, NULL, NULL
            );

      IF C_REFITEM%ISOPEN THEN
          BEGIN
            CLOSE C_REFITEM;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_SHIPPING_DTL_REGULAR%ISOPEN THEN
          BEGIN
            CLOSE C_SHIPPING_DTL_REGULAR;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_SHIPPING_DTL_SAMPLE%ISOPEN THEN
          BEGIN
            CLOSE C_SHIPPING_DTL_SAMPLE;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;

      IF C_INVOICE_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_INVOICE_DTL;

            EXCEPTION
              WHEN OTHERS THEN
                NULL;
          END;
      END IF;


END T_SP_RKP4290;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290                      
** ************************************************************************* */
/
EXIT
