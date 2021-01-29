**********************************************************************
* Author: T.Meyer, 04.10.20
**********************************************************************
*
* Query Transport Tool
* Check if a certain query is on which transports by technical name
* Check if a which queries are on a specific transport
* Identify query element
*
**********************************************************************
* Change log
**********************************************************************
* 04.10.20 TM initial version
**********************************************************************
REPORT z_qtt.

DATA: lv_elem   TYPE sysuuid_25.
DATA: lv_tr     TYPE trkorr.

SELECTION-SCREEN BEGIN OF BLOCK b1k1 WITH FRAME.
PARAMETERS: p_query TYPE rszcompid.

SELECTION-SCREEN END OF BLOCK b1k1.

SELECTION-SCREEN BEGIN OF BLOCK b1k2 WITH FRAME.
SELECT-OPTIONS: s_tr FOR lv_tr,
                s_el FOR lv_elem.

SELECTION-SCREEN END OF BLOCK b1k2.



"Check Query on transport requests
IF p_query <> ''.
  zcl_qtt=>get_query_on_requests( p_query ).
  RETURN.
ENDIF.

"Identify query element
IF s_el-low <> ''.
  zcl_qtt=>get_elem_information( s_el[] ).
"Content of transport requests
ELSEIF s_tr-low <> ''.
  zcl_qtt=>get_request_elem_content( s_tr[] ).
ENDIF.
