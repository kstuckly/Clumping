CREATE OR REPLACE PROCEDURE T_SP_RKP4290_HTSBOOST

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_HTSBOOST.SQL                                    **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_HTSBOOST                                   **
**                                                                           **
**  AUTHOR   : KAVIN J. STUCKLY                                              **
**  CREATED  : 13 MARCH 2003                                                 **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedures T_SP_RKP4290.  This procedure will load the        **
**             Entry Detail table using data from the Htsboost detail table  **
**             (Components at Item on Contract Detail).                      **
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
   p_load_status          OUT VARCHAR2,
   p_orig_first_cost   IN     INVOICE_DTL.PRICE%TYPE,
   p_discount_rate     IN     ADJUST_DTL.VALUE%TYPE
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
   c_rate_1               ENTRY_DTL.RATE_1%TYPE;
   c_customs_val_1        ENTRY_DTL.CUSTOMS_VAL_1%TYPE;
   c_rate_2               ENTRY_DTL.RATE_2%TYPE;
   c_customs_val_2        ENTRY_DTL.CUSTOMS_VAL_2%TYPE;
   c_rate_4               ENTRY_DTL.RATE_4%TYPE;
   c_customs_val_4        ENTRY_DTL.CUSTOMS_VAL_4%TYPE;
   c_rate_5               ENTRY_DTL.RATE_5%TYPE;
   c_customs_val_5        ENTRY_DTL.CUSTOMS_VAL_5%TYPE;
   c_component_desc       HTSBOOST_DTL.ITEM_DESC%TYPE;
   c_export_country       HTSBOOST_DTL.EXPORT_COUNTRY%TYPE;
   c_item_first_cost      HTSBOOST_DTL.CUSTOMS_VAL%TYPE;
   c_item_first_cost_sav  HTSBOOST_DTL.CUSTOMS_VAL%TYPE;
   c_hts_line_no          HTSBOOST_DTL.LINE_NO%TYPE;
   c_htsboost_ltn         HTSBOOST_DTL.LINK_TBL_NAME%TYPE;
   c_htsboost_lkv         HTSBOOST_DTL.LINK_KEY_VAL%TYPE;
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

   c_spec_prog11          HTS.SPEC_PROG11%TYPE;
   c_spec_prog12          HTS.SPEC_PROG12%TYPE;
   c_spec_prog13          HTS.SPEC_PROG13%TYPE;
   c_spec_prog14          HTS.SPEC_PROG14%TYPE;
   c_spec_prog15          HTS.SPEC_PROG15%TYPE;
   c_spec_prog16          HTS.SPEC_PROG16%TYPE;
   c_spec_prog17          HTS.SPEC_PROG17%TYPE;
   c_spec_prog18          HTS.SPEC_PROG18%TYPE;
   c_spec_prog19          HTS.SPEC_PROG19%TYPE;
   c_spec_prog20          HTS.SPEC_PROG20%TYPE;

   c_spec_prog21          HTS.SPEC_PROG21%TYPE;
   c_spec_prog22          HTS.SPEC_PROG22%TYPE;
   c_spec_prog23          HTS.SPEC_PROG23%TYPE;
   c_spec_prog24          HTS.SPEC_PROG24%TYPE;
   c_spec_prog25          HTS.SPEC_PROG25%TYPE;
   c_spec_prog26          HTS.SPEC_PROG26%TYPE;
   c_spec_prog27          HTS.SPEC_PROG27%TYPE;
   c_spec_prog28          HTS.SPEC_PROG28%TYPE;
   c_spec_prog29          HTS.SPEC_PROG29%TYPE;
   c_spec_prog30          HTS.SPEC_PROG30%TYPE;

   c_hts_spec_prog        COUNTRY.HTS_SPEC_PROG%TYPE;

   p3                     NUMBER;
   c_discount_amt         NUMBER;
   c_component_disc       NUMBER;
   c_component_cnt        NUMBER;
   c_component_loop_cnt   NUMBER;

   c_load_status          VARCHAR2(10);


   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */


   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */


   HTSBOOST_SKIP          EXCEPTION;
   HTSBOOST_ERROR         EXCEPTION;


/* ************************************************************************* **
**                 CURSORS                                                   **
** ************************************************************************* */

   CURSOR C_HTSBOOST_DTL IS
   SELECT HTS_NO, LINE_NO, NVL(ITEM_DESC, c_item_desc), NVL(CUSTOMS_VAL, 0),
          EXPORT_COUNTRY

     FROM HTSBOOST_DTL

    WHERE OWNER         = p_owner
      AND LINK_TBL_NAME = c_htsboost_ltn
      AND LINK_KEY_VAL  = c_htsboost_lkv
      AND PRECLASS_NO   = p_item
      AND ITEM          = p_item
    ORDER BY LINE_NO;

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
            ('T_SP_RKP4290_HTSBOOST', p_entry_no, '800', 'IF', USER,
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
      IF p_load_status IN ('ERROR', 'FALSE') THEN
          RETURN(p_load_status);
      ELSE
          RETURN(l_status);
      END IF;

  ELSE
      IF p_load_status IN ('ERROR', 'FALSE', 'WARN') THEN
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
             BEGIN OF LOCAL STORED PROCEDURE INSERT_ENTRY_DTL
** ************************************************************************* */

PROCEDURE INSERT_ENTRY_DTL

IS

BEGIN

  /* *******************************************************************
   * Kavin 2/21/2013 Item Duty Amt will now be stored in both 
   * ITEM_DUTY_AMT and HTS_DUTY_AMT.  The Item Duty Amount is the
   * Quoted Duty or Estimated Duty at the time the entry is created.
   * After entry is cleared by U.S. Customs, program Rkp4296 will
   * update the ITEM_DUTY_AMT with the "real" duty amt. Since
   * HTS_DUTY_AMT contains Quoted Duty, Rockport Impromptu will be able to
   * report difference between estimated duty * and real duty at item 
   * level of the entry.
   * ******************************************************************* */

  INSERT INTO ENTRY_DTL
         (OWNER, ENTRY_NO, INVOICE_NO, ITEM, LINE_NO, ITEM_DESC,
          REF_NO, QTY_ENTERED, QTY_UM, ITEM_DUTY_AMT, UNIT_DUTY,
          ORIGIN_COUNTRY, HTS_NO, UNIT_EXT_IND, RATE_1,
          CUSTOMS_VAL_1, RATE_2, CUSTOMS_VAL_2, RATE_3,
          CUSTOMS_VAL_3, RATE_4, CUSTOMS_VAL_4, RATE_5,
          CUSTOMS_VAL_5, RATE_6, CUSTOMS_VAL_6, RATE_7,
          CUSTOMS_VAL_7, EDI_IND, QTY_AVAIL_DB, P_LINK_KEY_VAL,
          LAST_USER,     LAST_UPDATE, LAST_ACTIVITY,
          HTS_DUTY_AMT
         )

   VALUES(p_owner, p_entry_no, p_invoice_no_wk, p_item, p_line_no,
          c_item_desc, p_ref_no, p_qty_entered, p_qty_um,
          c_item_duty_amt, c_unit_duty, c_origin_country, c_hts_no,
          'E', c_rate_1, c_customs_val_1, c_rate_2, c_customs_val_2,
          p_rate_3, p_customs_val_3, c_rate_4, c_customs_val_4,
          c_rate_5, c_customs_val_5, p_rate_6, p_customs_val_6,
          p_rate_7, p_customs_val_7, p_edi_ind,
          p_qty_avail_db,
          p_entry_no || ' + ' || p_invoice_no_wk || ' + ' || p_item || ' + ' ||
                       TO_CHAR(p_line_no, 'FM99999.09'),
          'RKP4290', SYSDATE, 'A',
          c_item_duty_amt
         );

  EXCEPTION
    WHEN OTHERS THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
               ('T_SP_RKP4290_HTSBOOST', p_entry_no, '801', 'INSERT', USER,
                'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                'B/L ' || p_bill_of_lading_no,
                'Container ' || LTRIM(RTRIM(p_equip_id)),
                'Item ' || p_item || ' HTS# ' ||
                      c_hts_no,
                'Error inserting component into Entry Dtl, '
                      || 'Entry Dtl Line No ' || p_line_no
               );

     RAISE HTSBOOST_ERROR;
END;

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE INSERT_ENTRY_DTL
** ************************************************************************* */



/* ************************************************************************* **
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_HTSBOOST      **
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

  c_load_status    :=  'TRUE';

  c_htsboost_ltn   :=  'CONTRACT - ' || p_ref_no;
  c_htsboost_lkv   :=  p_ref_no || ' + ' || p_ref_no || ' + ' || p_item || ' + %';


  BEGIN
    SELECT LINK_TBL_NAME, LINK_KEY_VAL

      INTO c_htsboost_ltn, c_htsboost_lkv

      FROM HTSBOOST_DTL

     WHERE OWNER            = p_owner
       AND LINK_TBL_NAME    = c_htsboost_ltn
       AND LINK_KEY_VAL  LIKE c_htsboost_lkv
       AND PRECLASS_NO      = p_item
       AND ITEM             = p_item
       AND ROWNUM           = 1;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_load_status := SET_LOAD_STATUS('FALSE');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '802', 'SELECT',
               USER, 'CF', TO_CHAR(SQLCODE), 'INF', SQLERRM,
               'Item '     || p_item,
               'Invoice# ' || LTRIM(RTRIM(p_invoice_no)),
               'Contract Item Component Information is missing.',
               'Please add HTS Booster to Item on Contract ' || p_ref_no
               );

        RAISE HTSBOOST_SKIP;


      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '803', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Item '  || p_item,
               'LTN ' || c_htsboost_ltn,
               'LKV ' || c_htsboost_lkv,
               'Error retrieving data from Htsboost detail'
              );

        RAISE HTSBOOST_ERROR;
  END;


  BEGIN
    SELECT NVL(SUM(NVL(CUSTOMS_VAL, 0)), 0)
      INTO c_item_first_cost

      FROM HTSBOOST_DTL

     WHERE OWNER          = p_owner
       AND LINK_TBL_NAME  = c_htsboost_ltn
       AND LINK_KEY_VAL   = c_htsboost_lkv
       AND PRECLASS_NO    = p_item
       AND ITEM           = p_item;

    IF p_price != c_item_first_cost THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '804', 'COMPARE', USER,
               'CW', '1', 'INF', NULL,
               'Item '     || p_item,
               'Invoice# ' || LTRIM(RTRIM(p_invoice_no)),
               'Contract Item Component total value different from Invoice price.',
               'Please update HTS Booster at Item on Contract '|| p_ref_no
               );
    END IF;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '805', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || p_item,
               'Container ' || LTRIM(RTRIM(p_equip_id)) || ', Contract ' || p_ref_no,
               'SQL error while calculating Contract Item Component total value.'
               );

        RAISE HTSBOOST_ERROR;
  END;


  c_hts_no        := NULL;

  c_item_duty_amt := 0;
  c_unit_duty     := 0;


  IF (p_discount_rate != 0 AND c_item_first_cost > 0) THEN
/*    c_item_first_cost := ROUND( c_item_first_cost + ROUND((c_item_first_cost * p_discount_rate), 3), 3); */

      c_item_first_cost := c_item_first_cost + TRUNC( (c_item_first_cost * p_discount_rate), 3);

      /*------------------------------------------*/
      /* CRQ0125835 - Begin Change                */
      /*   Kavin - 7/30/2014 - Added following    */
      /*   so c_customs_val_1 would contain       */
      /*   correct extended value when adjustment */
      /*   is applied to Finished Good            */
      /*------------------------------------------*/

      c_customs_val_1   := c_item_first_cost * p_qty_entered;

      /*------------------------------------------*/
      /* CRQ0125835 - End Change                  */
      /*------------------------------------------*/

  END IF;


  /*--------------------*/
  /* Finished Good line */
  /* for components     */
  /*--------------------*/

  c_rate_1        := c_item_first_cost;
  c_customs_val_4 := c_customs_val_1;
  c_customs_val_5 := c_customs_val_1;

  INSERT_ENTRY_DTL();


  /*----------------------------*/
  /* OVC - Original Vendor Cost */
  /*  Ref No booster            */
  /*----------------------------*/
  BEGIN
    IF (p_orig_first_cost != 0) THEN
        T_SP_RKP4290_REFITEM(p_entry_no,
                             p_owner,
                             'OVC',
                             p_invoice_no_wk,
                             NULL,
                             p_item,
                             p_line_no,
                             NULL,
                             NULL,
                             NULL,
                             p_orig_first_cost,
                             c_load_status );

        p_load_status := SET_LOAD_STATUS(c_load_status);

        IF c_load_status = 'ERROR' THEN
            RAISE HTSBOOST_ERROR;
        END IF;
    END IF;
  END;


  BEGIN
    SELECT NVL(COUNT(rowid), 0)
      INTO c_component_cnt

      FROM HTSBOOST_DTL

     WHERE OWNER               = p_owner
       AND LINK_TBL_NAME       = c_htsboost_ltn
       AND LINK_KEY_VAL        = c_htsboost_lkv
       AND PRECLASS_NO         = p_item
       AND ITEM                = p_item
       AND NVL(CUSTOMS_VAL, 0) > 0;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '806', 'SELECT', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || p_item,
               'Container ' || LTRIM(RTRIM(p_equip_id)) || ', Contract ' || p_ref_no,
               'SQL error while calculating Contract Item Component total value.'
               );

        RAISE HTSBOOST_ERROR;
  END;


  BEGIN
    OPEN C_HTSBOOST_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '807', 'OPEN', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || p_item,
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'SQL error while Opening Htsboost Detail cursor'
               );
        RAISE HTSBOOST_ERROR;

  END;

  c_hts_line_no    := 0;
  c_component_disc := 0;

  IF (p_discount_rate != 0) THEN
  /*  c_discount_amt := ROUND((p_orig_first_cost * p_discount_rate), 3); */
      c_discount_amt := TRUNC((p_orig_first_cost * p_discount_rate), 3);
  ELSE
      c_discount_amt := 0;
  END IF;

  c_component_loop_cnt := 0;

  <<L_4>>
  LOOP
    c_hts_no          := NULL;
    c_hts_line_no     := NULL;
    c_component_desc  := NULL;
    c_item_first_cost := 0;
    c_export_country  := NULL;

    BEGIN
      FETCH C_HTSBOOST_DTL
       INTO c_hts_no, c_hts_line_no, c_component_desc,
            c_item_first_cost, c_export_country;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_HTSBOOST', p_entry_no, '808', 'FETCH', USER,
                 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Item '  || p_item,
                 'LKV ' || c_htsboost_lkv,
                 'SQL Error retrieving data from Htsboost detail cursor'
                 );

          RAISE HTSBOOST_ERROR;
    END;

    EXIT L_4 WHEN C_HTSBOOST_DTL%NOTFOUND;

/* ************************************************************************* */

    c_customs_val_1      := c_item_first_cost;

    c_component_loop_cnt := c_component_loop_cnt + 1;

    T_SP_RKP4290_OGA('PRECLASS', p_owner, p_entry_no, p_bill_of_lading_no,
                     p_import_date, c_export_country, p_item,
                     c_hts_no, c_hts_line_no, p_foc_sample,
                     p_origin_ctry_sav, p_med_check, c_load_status);

    p_load_status := SET_LOAD_STATUS(c_load_status);

    IF p_load_status = 'ERROR' THEN
        RAISE HTSBOOST_ERROR;
    END IF;

    p_line_no             := TRUNC(p_line_no) + (c_hts_line_no - TRUNC(c_hts_line_no));

    c_item_desc           := c_component_desc;
    c_rate_2              := 0;
    c_customs_val_2       := 0;

    c_rate_4              := 0;
    c_customs_val_4       := 0;
    c_rate_5              := 0;
    c_customs_val_5       := 0;
    c_origin_country      := c_export_country;
    c_unit_duty           := 0;
    c_item_first_cost_sav := 0;

    IF (c_component_loop_cnt = 1) THEN
        c_item_duty_amt :=  p_item_duty_amt;
        c_unit_duty     :=  p_unit_duty;
    else
        c_item_duty_amt :=  0;
        c_unit_duty     :=  0;
    END IF;


    IF p_item_gsp_status = 'Y' THEN
        BEGIN
          c_hts_spec_prog := NULL;

          SELECT HTS_SPEC_PROG
            INTO c_hts_spec_prog
            FROM COUNTRY
           WHERE COUNTRY_CODE = c_export_country;


          IF c_hts_spec_prog IN ('A+', 'A') THEN
              BEGIN
                c_spec_prog1  := NULL;
                c_spec_prog2  := NULL;
                c_spec_prog3  := NULL;
                c_spec_prog4  := NULL;
                c_spec_prog5  := NULL;
                c_spec_prog6  := NULL;
                c_spec_prog7  := NULL;
                c_spec_prog8  := NULL;
                c_spec_prog9  := NULL;
                c_spec_prog10 := NULL;
                c_spec_prog11 := NULL;
                c_spec_prog12 := NULL;
                c_spec_prog13 := NULL;
                c_spec_prog14 := NULL;
                c_spec_prog15 := NULL;
                c_spec_prog16 := NULL;
                c_spec_prog17 := NULL;
                c_spec_prog18 := NULL;
                c_spec_prog19 := NULL;
                c_spec_prog20 := NULL;
                c_spec_prog21 := NULL;
                c_spec_prog22 := NULL;
                c_spec_prog23 := NULL;
                c_spec_prog24 := NULL;
                c_spec_prog25 := NULL;
                c_spec_prog26 := NULL;
                c_spec_prog27 := NULL;
                c_spec_prog28 := NULL;
                c_spec_prog29 := NULL;
                c_spec_prog30 := NULL;

                SELECT SPEC_PROG1,  SPEC_PROG2,  SPEC_PROG3,
                       SPEC_PROG4,  SPEC_PROG5,  SPEC_PROG6,
                       SPEC_PROG7,  SPEC_PROG8,  SPEC_PROG9,
                       SPEC_PROG10, SPEC_PROG11, SPEC_PROG12,
                       SPEC_PROG13, SPEC_PROG14, SPEC_PROG15,
                       SPEC_PROG16, SPEC_PROG17, SPEC_PROG18,
                       SPEC_PROG19, SPEC_PROG20, SPEC_PROG21,
                       SPEC_PROG22, SPEC_PROG23, SPEC_PROG24,
                       SPEC_PROG25, SPEC_PROG26, SPEC_PROG27,
                       SPEC_PROG28, SPEC_PROG29, SPEC_PROG30

                  INTO c_spec_prog1,  c_spec_prog2,  c_spec_prog3,
                       c_spec_prog4,  c_spec_prog5,  c_spec_prog6,
                       c_spec_prog7,  c_spec_prog8,  c_spec_prog9,
                       c_spec_prog10, c_spec_prog11, c_spec_prog12,
                       c_spec_prog13, c_spec_prog14, c_spec_prog15,
                       c_spec_prog16, c_spec_prog17, c_spec_prog18,
                       c_spec_prog19, c_spec_prog20, c_spec_prog21,
                       c_spec_prog22, c_spec_prog23, c_spec_prog24,
                       c_spec_prog25, c_spec_prog26, c_spec_prog27,
                       c_spec_prog28, c_spec_prog29, c_spec_prog30

                  FROM HTS

                 WHERE HTS_NO           = c_hts_no
                   AND BEG_EFFECT_DATE <= p_import_date
                   AND END_EFFECT_DATE >= p_import_date;

                IF (c_spec_prog1  IN ('A', 'A+')) OR
                   (c_spec_prog2  IN ('A', 'A+')) OR
                   (c_spec_prog3  IN ('A', 'A+')) OR
                   (c_spec_prog4  IN ('A', 'A+')) OR
                   (c_spec_prog5  IN ('A', 'A+')) OR
                   (c_spec_prog6  IN ('A', 'A+')) OR
                   (c_spec_prog7  IN ('A', 'A+')) OR
                   (c_spec_prog8  IN ('A', 'A+')) OR
                   (c_spec_prog9  IN ('A', 'A+')) OR
                   (c_spec_prog10 IN ('A', 'A+')) OR
                   (c_spec_prog11 IN ('A', 'A+')) OR
                   (c_spec_prog12 IN ('A', 'A+')) OR
                   (c_spec_prog13 IN ('A', 'A+')) OR
                   (c_spec_prog14 IN ('A', 'A+')) OR
                   (c_spec_prog15 IN ('A', 'A+')) OR
                   (c_spec_prog16 IN ('A', 'A+')) OR
                   (c_spec_prog17 IN ('A', 'A+')) OR
                   (c_spec_prog18 IN ('A', 'A+')) OR
                   (c_spec_prog19 IN ('A', 'A+')) OR
                   (c_spec_prog20 IN ('A', 'A+')) OR
                   (c_spec_prog21 IN ('A', 'A+')) OR
                   (c_spec_prog22 IN ('A', 'A+')) OR
                   (c_spec_prog23 IN ('A', 'A+')) OR
                   (c_spec_prog24 IN ('A', 'A+')) OR
                   (c_spec_prog25 IN ('A', 'A+')) OR
                   (c_spec_prog26 IN ('A', 'A+')) OR
                   (c_spec_prog27 IN ('A', 'A+')) OR
                   (c_spec_prog28 IN ('A', 'A+')) OR
                   (c_spec_prog29 IN ('A', 'A+')) OR
                   (c_spec_prog30 IN ('A', 'A+')) THEN
                     p_edi_ind := 'Y';
                ELSE
                     p_edi_ind := 'A';
                END IF;


                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    p_edi_ind := 'Y';

          WHEN TOO_MANY_ROWS THEN
                    p_load_status := SET_LOAD_STATUS('FALSE');

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290_HTSBOOST', p_entry_no, '809',
                           'SELECT', USER, 'CF', TO_CHAR(SQLCODE), 'ERR',
                           SQLERRM,
                           'HTS # ' || c_hts_no,
                           'Begin Effect Date <= ' || p_import_date,
                           'End Effect Date >= ' || p_import_date,
                           'Overlapping Date Ranges exist in HTS table '
                                 || 'for specified tariff'
                           );

                  WHEN OTHERS THEN
                    p_load_status := 'ERROR';

                    p3 := shrmsglog_pkg.snd_log_msg
                          ('T_SP_RKP4290_HTSBOOST', p_entry_no, '810',
                           'SELECT', USER, 'CE', TO_CHAR(SQLCODE), 'ERR',
                           SQLERRM,
                           'B/L ' || p_bill_of_lading_no,
                           'Container ' || LTRIM(RTRIM(p_equip_id)),
                           'Item ' || p_item || ' Contract ' || p_ref_no,
                           'Error selecting HTS special programs for '
                                 || 'HTS# ' || c_hts_no
                          );

                    RAISE HTSBOOST_ERROR;

              END;

          END IF;   /* IF c_hts_spec_prog IN ('A+', 'A') */

          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              p_edi_ind := 'Y';

            WHEN OTHERS THEN
              p_load_status := 'ERROR';

              p3 := shrmsglog_pkg.snd_log_msg
                    ('T_SP_RKP4290_HTSBOOST', p_entry_no, '811', 'SELECT',
                     USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                     'B/L ' || p_bill_of_lading_no,
                     'Container ' || LTRIM(RTRIM(p_equip_id)),
                     'Item ' || p_item || ' Contract ' || p_ref_no,
                     'Error selecting Country GSP Ind for country'
                          || ' code ' || c_export_country
                     );
              RAISE HTSBOOST_ERROR;

        END;

    END IF;    /* IF p_item_gsp_status = 'Y' */


    c_item_first_cost_sav := c_item_first_cost;

    IF (p_discount_rate != 0) THEN
        IF (c_component_cnt = 1) THEN
            c_customs_val_1 := c_item_first_cost + c_discount_amt;

            c_component_cnt := c_component_cnt - 1;

        ELSIF (c_item_first_cost > 0 AND c_component_cnt > 1 and c_discount_amt != 0) THEN
            /* c_component_disc := ROUND( (c_item_first_cost * p_discount_rate), 3);  */
               c_component_disc := TRUNC( (c_item_first_cost * p_discount_rate), 3);

               IF (ABS(c_component_disc) > ABS(c_discount_amt)) THEN
                   c_customs_val_1 := c_item_first_cost + c_discount_amt;
                   c_discount_amt  := 0;
               ELSE
                 /*c_customs_val_1 := ROUND( (c_item_first_cost * (1 + p_discount_rate)), 3); */
                   c_customs_val_1 := TRUNC( (c_item_first_cost * (1 + p_discount_rate)), 3);


                   /* ****************************************************************** **
                   Kavin 9/9/2010 - Following statement was revised so discount would be
                                     applied correctly on the last component.
                   ** ****************************************************************** */
                   c_discount_amt  := c_discount_amt + (c_item_first_cost - c_customs_val_1);

               END IF;

               c_component_cnt := c_component_cnt - 1;
        END IF;

    END IF;


    /*--------------------*/
    /* Component line     */
    /*--------------------*/

    c_rate_1        := c_customs_val_1;  
    c_customs_val_4 := c_customs_val_1;
    c_customs_val_5 := c_customs_val_1;

    c_customs_val_1 := 0;

    INSERT_ENTRY_DTL();


    BEGIN
      IF (p_discount_rate  != 0) THEN
          T_SP_RKP4290_REFITEM(p_entry_no,
                               p_owner,
                               'OVC',
                               p_invoice_no_wk,
                               NULL,
                               p_item,
                               p_line_no,
                               NULL,
                               NULL,
                               NULL,
                               c_item_first_cost_sav,
                               c_load_status );

          p_load_status := SET_LOAD_STATUS(c_load_status);

          IF c_load_status = 'ERROR' THEN
              RAISE HTSBOOST_ERROR;
          END IF;
      END IF;
    END;


    IF p_edi_ind = 'A' THEN
        p_load_status := SET_LOAD_STATUS('WARN');

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '812', 'IF', USER,
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
    CLOSE C_HTSBOOST_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '813', 'CLOSE', USER,
               'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || p_bill_of_lading_no,
               'Item ' || p_item,
               'Container ' || LTRIM(RTRIM(p_equip_id)),
               'Error closing Htsboost Detail cursor'
               );

        RAISE HTSBOOST_ERROR;
  END;

  p_line_no     := TRUNC(p_line_no);


  EXCEPTION
    WHEN HTSBOOST_SKIP OR HTSBOOST_ERROR THEN
      p_line_no     := TRUNC(p_line_no);

      IF C_HTSBOOST_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_HTSBOOST_DTL;

            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

    WHEN OTHERS THEN
      p_load_status := 'ERROR';
      p_line_no     := TRUNC(p_line_no);

      p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_HTSBOOST', p_entry_no, '814', 'EXCEPTION',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'B/L ' || LTRIM(RTRIM(p_bill_of_lading_no)),
               'Item No ' || p_item,
               'Entry Detail Line No ' || p_line_no,
               'Unexpected SQL error in HTSBOOST stored procedure'
              );

      IF C_HTSBOOST_DTL%ISOPEN THEN
          BEGIN
            CLOSE C_HTSBOOST_DTL;

            EXCEPTION
              WHEN OTHERS THEN NULL;
          END;
      END IF;

END T_SP_RKP4290_HTSBOOST;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_HTSBOOST
** ************************************************************************* */
/
EXIT
