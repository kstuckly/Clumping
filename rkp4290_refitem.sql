
/* *** static char rscid[] = "@(#) [item] [version] [crtime] [author]"; *** */

CREATE OR REPLACE PROCEDURE T_SP_RKP4290_REFITEM

/* ************************************************************************* **
**  PROGRAM NAME   : RKP4290_REFITEM.SQL                                     **
**                                                                           **
**  MODULE NAME    : T_SP_RKP4290_REFITEM                                    **
**                                                                           **
**  AUTHOR   : KAVIN J STUCKLY                                               **
**  CREATED  : 16 AUGUST 2001                                                **
**  PURPOSE  : This procedure should be executed from within Stored          **
**             Procedures T_SP_RKP4290 T_SP_RKP4290_SAMPLE,                  **
**             T_SP_RKP4290_MXPK  and T_SP_RKP4290_HTSBOOST.  This stored    **
**             procedure is responsible for creating the following Ref No    **
**             booster at the Entry Detail level:                            **
**               EID - Equipment ID or Container that the item is in         **
**               MXP - Carton count associated with an item                  **
**               NCS - Line number on BOL containing the Free of Charge Item **
**               OVC - Original Vendor Cost from vendor invoice / components **
**                                                                           **
** ************************************************************************* */
(
   p_entry_no        IN  ENTRY_HDR.ENTRY_NO%TYPE,
   p_owner           IN  ENTRY_INV.OWNER%TYPE,
   p_reference_qlf   IN  REFITEM.REFERENCE_QLF%TYPE,
   p_entry_inv_no    IN  ENTRY_INV.INVOICE_NO%TYPE,
   p_ship_line_no    IN  SHIPPING_DTL.LINE_NO%TYPE,
   p_entry_item      IN  ENTRY_DTL.ITEM%TYPE,
   p_entry_line_no   IN  ENTRY_DTL.LINE_NO%TYPE,
   p_ship_equip_id   IN  SHIPPING_DTL.EQUIP_ID%TYPE,
   p_mxpk_id         IN  MATRIX_DTL.ROW_NAME%TYPE,
   p_pack_total      IN  NUMBER,
   p_orig_price      IN  INVOICE_DTL.PRICE%TYPE,
   p_load_status     OUT VARCHAR2
)


AS

/* ************************************************************************* **
** variables declaration.                                                    **
** ************************************************************************* */

   p3                     NUMBER;

   c_owner                REFITEM.OWNER%TYPE;
   c_link_tbl_name        REFITEM.LINK_TBL_NAME%TYPE;
   c_link_key_val         REFITEM.LINK_KEY_VAL%TYPE;
   c_ref_no               REFITEM.REF_NO%TYPE;
   c_aka_ind              REFITEM.AKA_IND%TYPE;
   c_effective_date       REFITEM.EFFECTIVE_DATE%TYPE;
   c_expiry_date          REFITEM.EXPIRY_DATE%TYPE;
   c_edi_ind              REFITEM.EDI_IND%TYPE;
   c_last_user            REFITEM.LAST_USER%TYPE;
   c_last_activity        REFITEM.LAST_ACTIVITY%TYPE;
   c_last_update          REFITEM.LAST_UPDATE%TYPE;

   REFITEM_ERROR          EXCEPTION;


/* ************************************************************************* **
**      BEGIN OF CONTROL AREA FOR STORED PROCEDRE T_SP_RKP4290_REFITEM       **
** ************************************************************************* */

BEGIN
  p_load_status    := 'TRUE';

  c_owner          := p_owner;
  c_link_tbl_name  := 'ENTRY - ' || RTRIM(p_entry_no);

  c_link_key_val   := RTRIM(p_entry_no) || ' + ' ||
		      LTRIM(RTRIM(p_entry_inv_no))
                      || ' + ' || LTRIM(RTRIM(p_entry_item)) || ' + ' ||
                      LTRIM(RTRIM(TO_CHAR(p_entry_line_no, '9999999999.9')));

  c_aka_ind        := NULL;
  c_effective_date := NULL;
  c_expiry_date    := NULL;
  c_edi_ind        := NULL;
  c_last_user      := 'RKP4290';
  c_last_activity  := 'A';
  c_last_update    := SYSDATE;


  IF p_reference_qlf = 'EID' THEN
      c_ref_no  := p_ship_equip_id;
  ELSIF p_reference_qlf = 'MXP' THEN
           c_ref_no := LTRIM(RTRIM(p_ship_equip_id)) || '/' ||
	   LTRIM(RTRIM(p_mxpk_id)) || '/' || TO_CHAR(p_pack_total);
  ELSIF p_reference_qlf = 'NCS' THEN
          c_ref_no := p_ship_line_no;
  ELSIF p_reference_qlf = 'OVC' THEN
          c_ref_no := TO_CHAR(p_orig_price, '099999.99');
  ELSE
      RAISE REFITEM_ERROR;

  END IF;


  INSERT INTO REFITEM
           (OWNER,
            LINK_TBL_NAME,
            LINK_KEY_VAL,
            REF_NO,
            REFERENCE_QLF,
            AKA_IND,
            EFFECTIVE_DATE,
            EXPIRY_DATE,
            EDI_IND,
            LAST_USER,
            LAST_UPDATE,
            LAST_ACTIVITY
           )

          VALUES (c_owner,
                  c_link_tbl_name,
                  c_link_key_val,
                  c_ref_no,
                  p_reference_qlf,
                  c_aka_ind,
                  c_effective_date,
                  c_expiry_date,
                  c_edi_ind,
                  c_last_user,
                  c_last_update,
                  c_last_activity
                 );

  EXCEPTION
    WHEN REFITEM_ERROR THEN
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_REFITEM', p_entry_no, '501', 'IF', USER,
             'CE', '1', 'ERR', NULL,
             'Internal Stored Procedure Error',
             'Reference Qlf of ' ||   p_reference_qlf || ' is invalid',
             'Reference Qlf must be EID, MXP, NCS or OVC',
             NULL
            );

    WHEN OTHERS THEN 
      p_load_status := 'ERROR';

      p3 := shrmsglog_pkg.snd_log_msg
            ('T_SP_RKP4290_REFITEM', p_entry_no, '502', 'INSERT', 
             USER, 'CE', TO_CHAR(SQLCODE), 'ERR', SQLERRM,
             'Ltn ' || RTRIM(c_link_tbl_name),
             'Lkv ' || RTRIM(c_link_key_val),
             'Ref No ' || RTRIM(c_ref_no),
             'Error Creating ' || p_reference_qlf ||
		      ' Ref No booster off Entry Detail'
             );


END T_SP_RKP4290_REFITEM;
/* ************************************************************************* **
             END OF STORED PROCEDURE T_SP_RKP4290_REFITEM
** ************************************************************************* */
/
EXIT
