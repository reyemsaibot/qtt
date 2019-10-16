*&---------------------------------------------------------------------*
*& Report z_qtt
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_qtt.

Data :lv_elem   TYPE sysuuid_25,
      lv_tr     TYPE trkorr.

SELECTION-SCREEN BEGIN OF BLOCK b1k2 WITH FRAME TITLE text-001.
  PARAMETERS:     p_query TYPE rszcompid.
  SELECT-OPTIONS: s_tr FOR lv_tr,
                  s_el FOR lv_elem.
SELECTION-SCREEN END OF BLOCK b1k2.

IF p_query NE ''.
  z_qtt=>get_query_on_requests( p_query ).
  exit.
ENDIF.

"Identify query element
IF s_el-low <> ''.
  z_qtt=>get_elem_information( s_el[] ).
"Content of transport requests
ELSEIF s_tr-low <> ''.
  z_qtt=>get_request_elem_content( s_tr[] ).
ENDIF.
