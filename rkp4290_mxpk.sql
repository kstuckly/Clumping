
/* *** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; *** */

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_MXPK

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_MXPK.SQL                                        **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_MXPK                                       **
**                                                                           **
**  AUTHOR   : KAVIN J STUCKLY                                               **
**  CREATED  : 06 DECEMBER 2000                                              **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedure T_SP_RKP4290.  This procedure is to load Mix Pack   **
**             carton counts associated with an entry in the ENTRY tables.   **
**                                                                           **
** ************************************************************************* */
(
   p_entry_no        IN   ENTRY_HDR.ENTRY_NO%TYPE,
   p_owner           IN   ENTRY_INV.OWNER%TYPE,
   p_load_status     OUT  VARCHAR2
)


AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   p3                     NUMBER;
   c_sql_ret_code         NUMBER;
   c_pack_total           NUMBER;
   c_entry_dtl_count      NUMBER;
   c_orig_first_cost      NUMBER;

   c_foc_ind              VARCHAR2(5);
   c_load_status          VARCHAR2(10);

   c_invoice_no           ENTRY_DTL.INVOICE_NO%TYPE;
   c_item                 ENTRY_DTL.ITEM%TYPE;
   c_line_no              ENTRY_DTL.LINE_NO%TYPE;
   c_ref_no               ENTRY_DTL.REF_NO%TYPE;
   c_prev_ref_no          ENTRY_DTL.REF_NO%TYPE;
   c_bill_of_lading_no    ENTRY_INV.BILL_OF_LADING_NO%TYPE;

   c_link_key_val         REFITEM.LINK_KEY_VAL%TYPE;



   /* ************************************************************************** **
   **                   Constants for Performance Tunning                        **
   ** ************************************************************************** */

   g_reference_qlf_mxp    CONSTANT REFITEM.REFERENCE_QLF%TYPE    := 'MXP';
   g_matrix_id_ctn        CONSTANT MATRIX_DTL.MATRIX_ID%TYPE     := 'CTN';
   g_edi_ind_y            CONSTANT SHIPPING_DTL.EDI_IND%TYPE     := 'Y';
   g_matrix_ltn           CONSTANT MATRIX_DTL.LINK_TBL_NAME%TYPE := 'SHIPPING - ';
   g_refitem_ltn          CONSTANT REFITEM.LINK_TBL_NAME%TYPE    := 'ENTRY - ';

   /* ************************************************************************** **
   **                   End of Constants for Performance Tunning                 **
   ** ************************************************************************** */


   NO_ENTRY_DTL_MXPK      EXCEPTION;
   MIX_PACK_ERROR         EXCEPTION;


/* ************************************************************************* **
**                 CURSORS                                                   **
** ************************************************************************* */

   CURSOR C_ENTRY_DTL IS
   SELECT ED.INVOICE_NO,
          ITEM,
          LINE_NO,
          REF_NO,
          BILL_OF_LADING_NO,
          SUBSTR(ED.INVOICE_NO, 1, 4)

     FROM ENTRY_DTL ED, ENTRY_INV EI

    WHERE ED.OWNER                 = p_owner
      AND ED.ENTRY_NO              = p_entry_no
      AND EI.OWNER                 = ED.OWNER
      AND EI.ENTRY_NO              = ED.ENTRY_NO
      AND LINE_NO                  = TRUNC(LINE_NO)

    GROUP BY ED.INVOICE_NO, BILL_OF_LADING_NO, REF_NO, ITEM, LINE_NO

    ORDER BY ED.INVOICE_NO, BILL_OF_LADING_NO, REF_NO, ITEM;

/* ************************************************************************* */

   CURSOR C_REFITEM_MXPK IS
   SELECT REF_NO,

          SUBSTR(LINK_KEY_VAL, (INSTR(LINK_KEY_VAL, ' + ', 1, 1) + 3),
                   (INSTR(LINK_KEY_VAL, ' + ', 1, 2) -
		    ((INSTR(LINK_KEY_VAL, ' + ', 1, 1)) + 3))),

          SUBSTR(LINK_KEY_VAL, (INSTR(LINK_KEY_VAL, ' + ', 1, 2) + 3),
                   (INSTR(LINK_KEY_VAL, ' + ', 1, 3) -
                    ((INSTR(LINK_KEY_VAL, ' + ', 1, 2)) + 3))),

          SUBSTR(LINK_KEY_VAL, (INSTR(LINK_KEY_VAL, ' + ', 1, 3) + 3)),

          TO_NUMBER(RTRIM(SUBSTR(REF_NO, (INSTR(REF_NO,'/', 1, 2) + 1))))

     FROM REFITEM

    WHERE OWNER         = p_owner

      AND LINK_TBL_NAME = g_refitem_ltn || LTRIM(RTRIM(p_entry_no))

      AND REFERENCE_QLF = g_reference_qlf_mxp

    ORDER BY REF_NO;

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
            ('T_SP_RKP4290_SAMPLE', p_entry_no, '400', 'IF', USER,
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
             BEGIN OF LOCAL STORED PROCEDURE REGULAR_MIX_PACK              
** ************************************************************************* */

PROCEDURE REGULAR_MIX_PACK
   (
    l_invoice_no        IN  ENTRY_DTL.INVOICE_NO%TYPE,
    l_item              IN  ENTRY_DTL.ITEM%TYPE,
    l_line_no           IN  ENTRY_DTL.LINE_NO%TYPE,
    l_ref_no            IN  ENTRY_DTL.REF_NO%TYPE,
    l_bill_of_lading_no IN  ENTRY_INV.BILL_OF_LADING_NO%TYPE,
    l_sql_ret_code      OUT NUMBER
   )

IS

   c_foc_ind           VARCHAR2(5);
   c_link_key_val      MATRIX_DTL.LINK_KEY_VAL%TYPE;
   c_mxpk_id           MATRIX_DTL.ROW_NAME%TYPE;
   c_item              MATRIX_DTL.ROW_CODE%TYPE;
   c_equip_id          SHIPPING_DTL.EQUIP_ID%TYPE;
   c_pack_total        NUMBER;

   REGULAR_MIXPK_ERROR EXCEPTION;


/* ************************************************************************* **
**                 CURSOR                                                    **
** ************************************************************************* */

   CURSOR C_REGULAR_MATRIX_MXPK   IS
   SELECT UNIQUE LINK_KEY_VAL,
          ROW_NAME, 
          ROW_CODE,
          NVL(PACK_TOTAL, 0),
          SUBSTR(LINK_KEY_VAL, INSTR(LINK_KEY_VAL, '+', 1, 1) + 2)

     FROM MATRIX_DTL

    WHERE OWNER         = p_owner

      AND LINK_TBL_NAME = g_matrix_ltn || LTRIM(RTRIM(l_bill_of_lading_no))

      AND LINK_KEY_VAL || ROW_NO IN

           (SELECT LTRIM(RTRIM(l_bill_of_lading_no)) ||' + '||
                    LTRIM(RTRIM(EQUIP_ID)) || REF_NO

              FROM SHIPPING_DTL

             WHERE OWNER             = p_owner
               AND BILL_OF_LADING_NO = LTRIM(RTRIM(l_bill_of_lading_no))
               AND CONTRACT_NO       = LTRIM(RTRIM(l_ref_no))
               AND INVOICE_NO        = LTRIM(RTRIM(l_invoice_no))
               AND ITEM              = LTRIM(RTRIM(l_item))
           )

      AND MATRIX_ID        = g_matrix_id_ctn
      AND ROW_CODE         = LTRIM(RTRIM(l_item))
      AND RTRIM(ROW_NAME) IS NOT NULL
      AND RTRIM(ROW_NAME) <> g_matrix_id_ctn

    ORDER BY LINK_KEY_VAL, ROW_NAME, ROW_CODE;

/* ************************************************************************* */

BEGIN
  l_sql_ret_code := 0;

  BEGIN
    OPEN C_REGULAR_MATRIX_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        l_sql_ret_code := SQLCODE;

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '401', 'OPEN',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Entry - ' || LTRIM(RTRIM(p_entry_no)),
               'Error opening Finished Good Mix Pack cursor',
               NULL, NULL
              );

        RAISE REGULAR_MIXPK_ERROR;
  END;  

  <<L_1>>
  LOOP
     c_bill_of_lading_no := NULL;
     c_foc_ind           := NULL;

     BEGIN
       FETCH C_REGULAR_MATRIX_MXPK
        INTO c_link_key_val,
             c_mxpk_id,
             c_item,
             c_pack_total,
             c_equip_id;
      
       EXCEPTION
         WHEN OTHERS THEN
           p_load_status := 'ERROR';
    
           l_sql_ret_code := SQLCODE;

           p3 := shrmsglog_pkg.snd_log_msg
                 ('T_SP_RKP4290_MXPK', p_entry_no, '402', 'FETCH', 
                  USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                  'Entry ' || LTRIM(RTRIM(p_entry_no)),
                  'Item ' || RTRIM(l_item) || ', Invoice ' ||
                          RTRIM(l_invoice_no),
                  'Line ' || l_line_no || ', B/L ' ||
                          RTRIM(l_bill_of_lading_no),
                  'Error Retrieving Finished Good Mix Pack data'
                 );

          RAISE REGULAR_MIXPK_ERROR;
     END;

     EXIT L_1 WHEN C_REGULAR_MATRIX_MXPK%NOTFOUND;

     T_SP_RKP4290_REFITEM(p_entry_no,
                          p_owner,
                          'MXP',
                          l_invoice_no,
                          NULL,
                          l_item,     
                          l_line_no,
                          c_equip_id,
                          c_mxpk_id,
                          c_pack_total,
			  c_orig_first_cost,  
                          c_load_status);

     p_load_status := SET_LOAD_STATUS(c_load_status);

     if c_load_status = 'ERROR' THEN
         l_sql_ret_code := 1;
         RAISE REGULAR_MIXPK_ERROR;
     END IF;

  END LOOP L_1;


  BEGIN
    CLOSE C_REGULAR_MATRIX_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        l_sql_ret_code := SQLCODE;

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '403', 'CLOSE',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Closing Finished Good Mix Pack cursor',
               NULL, NULL, NULL
              );
  END;  


  EXCEPTION
    WHEN REGULAR_MIXPK_ERROR THEN
      BEGIN
        IF C_REGULAR_MATRIX_MXPK%ISOPEN THEN
            CLOSE C_REGULAR_MATRIX_MXPK;
        END IF;
    
        EXCEPTION
          WHEN OTHERS THEN NULL;
      END;
END; 

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE REGULAR_MIX_PACK                   
** ************************************************************************* */


/* ************************************************************************* **
             BEGIN OF LOCAL STORED PROCEDURE SAMPLE_MIX_PACK              
** ************************************************************************* */

PROCEDURE SAMPLE_MIX_PACK
   (
    l_invoice_no        IN  ENTRY_DTL.INVOICE_NO%TYPE,
    l_item              IN  ENTRY_DTL.ITEM%TYPE,
    l_line_no           IN  ENTRY_DTL.LINE_NO%TYPE,
    l_ref_no            IN  ENTRY_DTL.REF_NO%TYPE,
    l_bill_of_lading_no IN  ENTRY_INV.BILL_OF_LADING_NO%TYPE,
    l_sql_ret_code      OUT NUMBER
   )

IS

   c_link_key_val      MATRIX_DTL.LINK_KEY_VAL%TYPE;
   c_mxpk_id           MATRIX_DTL.ROW_NAME%TYPE;
   c_item              MATRIX_DTL.ROW_CODE%TYPE;
   c_equip_id          SHIPPING_DTL.EQUIP_ID%TYPE;
   c_pack_total        NUMBER;

   SAMPLE_MIXPK_ERROR EXCEPTION;

/* ************************************************************************* **
**                 CURSOR                                                    **
** ************************************************************************* */

   CURSOR C_SAMPLE_MATRIX_MXPK IS
   SELECT UNIQUE LINK_KEY_VAL,
	      ROW_NAME, 
          ROW_CODE,
          NVL(PACK_TOTAL, 0),
          SUBSTR(LINK_KEY_VAL, INSTR(LINK_KEY_VAL, '+', 1, 1) + 2)

     FROM MATRIX_DTL

    WHERE OWNER         = p_owner

      AND LINK_TBL_NAME = g_matrix_ltn || LTRIM(RTRIM(l_bill_of_lading_no))

      AND LINK_KEY_VAL || ROW_NO IN 

             (SELECT LTRIM(RTRIM(l_bill_of_lading_no)) || ' + ' ||
                      LTRIM(RTRIM(EQUIP_ID)) || REF_NO

                FROM SHIPPING_DTL
    
               WHERE OWNER             = p_owner
                 AND BILL_OF_LADING_NO = LTRIM(RTRIM(l_bill_of_lading_no))
                 AND ITEM              = LTRIM(RTRIM(l_item))
                 AND EDI_IND           = g_edi_ind_y
            )

      AND MATRIX_ID        = g_matrix_id_ctn
      AND ROW_CODE         = LTRIM(RTRIM(l_item))
      AND RTRIM(ROW_NAME) IS NOT NULL
      AND RTRIM(ROW_NAME) <> g_matrix_id_ctn

      ORDER BY LINK_KEY_VAL, ROW_NAME, ROW_CODE;

/* ************************************************************************* */


BEGIN
  l_sql_ret_code := 0;

  BEGIN
    OPEN C_SAMPLE_MATRIX_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '404', 'OPEN',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error opening Non-Chargeable Sample Mix Pack cursor',
               NULL, NULL, NULL
              );
  END;  
  
  <<L_1>>
  LOOP
    c_link_key_val := NULL;
    c_mxpk_id      := NULL;
    c_item         := NULL;
    c_equip_id     := NULL;
    c_pack_total   := 0;

    BEGIN
      FETCH C_SAMPLE_MATRIX_MXPK
       INTO c_link_key_val,
            c_mxpk_id,
            c_item,
            c_pack_total,
            c_equip_id;

      EXIT L_1 WHEN C_SAMPLE_MATRIX_MXPK%NOTFOUND;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';
      
          l_sql_ret_code := SQLCODE;

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_MXPK', p_entry_no, '405', 'FETCH', 
                 USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'B/L ' || LTRIM(RTRIM(l_bill_of_lading_no)),
                 'Item ' || LTRIM(RTRIM(l_item)),
                 'Error Retrieving Non-Chargeabl Sample Matrix data',
                 NULL                                                  
                );

          RAISE SAMPLE_MIXPK_ERROR;
     END; 


     T_SP_RKP4290_REFITEM(p_entry_no,
                          p_owner,
                          'MXP',
                          l_invoice_no,
                          NULL,
                          l_item,     
                          l_line_no,
                          c_equip_id,
                          c_mxpk_id,
                          c_pack_total,
                          c_orig_first_cost,
                          c_load_status);

     IF c_load_status = 'ERROR' THEN
          l_sql_ret_code := 1;
          RAISE SAMPLE_MIXPK_ERROR;
     END IF;

  END LOOP L_1;

  BEGIN
    CLOSE C_SAMPLE_MATRIX_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        l_sql_ret_code := SQLCODE;

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '406', 'CLOSE',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Closing Non-Chargeable Sample Mix Pack cursor',
               NULL, NULL, NULL
              );
  END;  
  

  EXCEPTION
    WHEN SAMPLE_MIXPK_ERROR THEN
      BEGIN
        IF C_SAMPLE_MATRIX_MXPK%ISOPEN THEN
            CLOSE C_SAMPLE_MATRIX_MXPK;
        END IF;

	EXCEPTION
	  WHEN OTHERS THEN NULL;
      END;
END; 

/* ************************************************************************* **
             END OF LOCAL STORED PROCEDURE SAMPLE_MIX_PACK             
** ************************************************************************* */


/* ************************************************************************* **
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_MXPK          **
** ************************************************************************* */

BEGIN
  c_sql_ret_code    := 0;
  p_load_status     := 'TRUE';
  c_load_status     := 'TRUE';
  c_orig_first_cost := 0;

  BEGIN
    OPEN C_ENTRY_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '407', 'OPEN',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Opening Entry Detail cursor',
               NULL, NULL, NULL
              );

        RAISE MIX_PACK_ERROR;
  END;  
  
  <<L_1>>
  LOOP
    c_bill_of_lading_no := NULL;
    c_foc_ind           := NULL;

    BEGIN
      FETCH C_ENTRY_DTL
       INTO c_invoice_no,
            c_item,
            c_line_no,
            c_ref_no,
            c_bill_of_lading_no,
            c_foc_ind;

      c_entry_dtl_count := C_ENTRY_DTL%ROWCOUNT;

      EXIT L_1 WHEN C_ENTRY_DTL%NOTFOUND;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_MXPK', p_entry_no, '408', 'FETCH', 
                 USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Error Retrieving data from Entry Detail cursor',
                 NULL, NULL, NULL
                );

          RAISE MIX_PACK_ERROR;
    END; 

    IF LTRIM(RTRIM(c_foc_ind)) = 'FOC/' THEN
        SAMPLE_MIX_PACK(c_invoice_no, c_item, c_line_no, c_ref_no,
                        c_bill_of_lading_no, c_sql_ret_code);
    ELSE
        REGULAR_MIX_PACK(c_invoice_no, c_item, c_line_no, c_ref_no,
                         c_bill_of_lading_no, c_sql_ret_code);
    END IF;

    IF (c_sql_ret_code != 0) THEN
        RAISE MIX_PACK_ERROR;
    END IF;

  END LOOP L_1;


  BEGIN
    CLOSE C_ENTRY_DTL;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '409', 'CLOSE',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Closing Entry Detail cursor',
               NULL, NULL, NULL
              );

        RAISE MIX_PACK_ERROR;
  END;  

  IF c_entry_dtl_count = 0 THEN
      RAISE NO_ENTRY_DTL_MXPK;
  END IF;

  c_prev_ref_no := 'DUMMY';

  BEGIN
    OPEN C_REFITEM_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '410', 'OPEN',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Opening MXPK Ref No booster cursor',
               NULL, NULL, NULL
              );

        RAISE MIX_PACK_ERROR;
  END;  


  <<L_2>>
  LOOP
    c_link_key_val := NULL;
    c_ref_no       := NULL;

    BEGIN
      FETCH C_REFITEM_MXPK
       INTO c_ref_no,
            c_invoice_no,
            c_item,
            c_line_no,
            c_pack_total;

      EXIT L_2 WHEN C_REFITEM_MXPK%NOTFOUND;

      EXCEPTION
        WHEN OTHERS THEN
          p_load_status := 'ERROR';

          p3 := shrmsglog_pkg.snd_log_msg
                ('T_SP_RKP4290_MXPK', p_entry_no, '411', 'FETCH', 
                 USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                 'Ltn ENTRY - ' || LTRIM(RTRIM(p_entry_no)),
                 'Reference Qlf: MXP',
                 'Error in Retrieving Mix Pack Ref No boosters',
                 NULL
                );

          RAISE MIX_PACK_ERROR;
     END; 


     IF (c_ref_no != c_prev_ref_no) THEN
         BEGIN
           UPDATE ENTRY_DTL

              SET QTY_AVAIL_DB = NVL(QTY_AVAIL_DB, 0) + c_pack_total

            WHERE OWNER        = p_owner
              AND ENTRY_NO     = LTRIM(RTRIM(p_entry_no))
              AND INVOICE_NO   = LTRIM(RTRIM(c_invoice_no))
              AND ITEM         = LTRIM(RTRIM(c_item))
              AND LINE_NO      = LTRIM(RTRIM(c_line_no));

           EXCEPTION
             WHEN OTHERS THEN 
               p_load_status := 'ERROR';

               p3 := shrmsglog_pkg.snd_log_msg
                     ('T_SP_RKP4290_MXPK', p_entry_no, '412', 'UPDATE', 
                      USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                      'Invoice No ' || c_invoice_no,
                      'Item ' || c_item,
                      'Line No ' || c_line_no,
                      'Error updating total cartons on Entry Detail'
                      );

                 RAISE MIX_PACK_ERROR;
           END;

       ELSE
           BEGIN
             DELETE FROM REFITEM

              WHERE OWNER         = p_owner

                AND LINK_TBL_NAME = g_refitem_ltn || LTRIM(RTRIM(p_entry_no))

                AND LINK_KEY_VAL  = LTRIM(RTRIM(p_entry_no)) || ' + ' ||
                                    LTRIM(RTRIM(c_invoice_no)) || ' + ' ||
                                    LTRIM(RTRIM(c_item)) || ' + ' ||
                                    LTRIM(TO_CHAR(c_line_no, '9999999999.9'))

                AND REFERENCE_QLF = g_reference_qlf_mxp;
     

             EXCEPTION
               WHEN OTHERS THEN 
                 p_load_status := 'ERROR';

                 p3 := shrmsglog_pkg.snd_log_msg
                      ('T_SP_RKP4290_MXPK', p_entry_no, '413', 'DELETE', 
                       USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
                       'Ltn ENTRY - ' || LTRIM(RTRIM(p_entry_no)),
                       'Lkv ' || LTRIM(RTRIM(p_entry_no)) || ' + ' ||
                                  LTRIM(RTRIM(c_invoice_no)) || ' + ' ||
                                  LTRIM(RTRIM(c_item)) || ' + ' ||
                                  LTRIM(RTRIM(c_line_no)),
                       'Reference Qlf: MXP',
                       'Error Deleting MXP Ref No Booster off Entry Dtl'
                      );

                 RAISE MIX_PACK_ERROR;
           END;
       END IF;

       c_prev_ref_no := c_ref_no;

    END LOOP L_2;


  BEGIN
    CLOSE C_REFITEM_MXPK;

    EXCEPTION
      WHEN OTHERS THEN
        p_load_status := 'ERROR';

        p3 := shrmsglog_pkg.snd_log_msg
              ('T_SP_RKP4290_MXPK', p_entry_no, '414', 'CLOSE',
               USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
               'Error Closing Entry Detail cursor',
               NULL, NULL, NULL
              );
  END;  


  EXCEPTION
    WHEN MIX_PACK_ERROR THEN
      BEGIN
        IF C_ENTRY_DTL%ISOPEN THEN
            CLOSE C_ENTRY_DTL;
        END IF;
 
        EXCEPTION
          WHEN OTHERS THEN NULL;
      END;

      BEGIN
        IF C_REFITEM_MXPK%ISOPEN THEN
            CLOSE C_REFITEM_MXPK;
        END IF;

        EXCEPTION
          WHEN OTHERS THEN NULL;
      END;


    WHEN NO_ENTRY_DTL_MXPK THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_MXPK', p_entry_no, '415', 'MIXPACK',
             USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'No Entry Detail rows found for Mix Pack processing',
             NULL, NULL, NULL
             );


    WHEN OTHERS THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_MXPK', p_entry_no, '416', 'MIXPACK',
             USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'Unexpected SQL error in MXPK stored procedure',
             NULL, NULL, NULL
             );

      BEGIN
        IF C_ENTRY_DTL%ISOPEN THEN
            CLOSE C_ENTRY_DTL;
        END IF;
 
        EXCEPTION
          WHEN OTHERS THEN NULL;
      END;

      BEGIN
        IF C_REFITEM_MXPK%ISOPEN THEN
            CLOSE C_REFITEM_MXPK;
        END IF;

        EXCEPTION
          WHEN OTHERS THEN NULL;
      END;

END T_SP_RKP4290_MXPK;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_MXPK                
** ************************************************************************* */
/
EXIT
