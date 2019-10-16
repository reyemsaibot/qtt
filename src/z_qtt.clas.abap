class Z_QTT definition
  public
  final
  create public .

public section.

  "! @parameter it_elem | Import table of ELEM objects
  class-methods GET_ELEM_INFORMATION
    importing
      !IT_ELEM type STANDARD TABLE .
   "! Get all transport requests with the imported query.
   "! @parameter i_query | Import query you want to search on transports
   class-methods GET_QUERY_ON_REQUESTS
    importing
      !I_QUERY type RSZCOMPID .
  class-methods GET_REQUEST_ELEM_CONTENT
    importing
      !IT_REQUEST type STANDARD TABLE .
protected section.
private section.

  types:
    BEGIN OF ty_elem,
        eltuid  TYPE sysuuid_25,
        mapname TYPE rszcompid,
        defaulthint TYPE rszdefaulthint,
       END OF ty_elem .

  types:
    BEGIN OF ty_trkorr,
         trkorr   TYPE trkorr,
         trstatus TYPE trstatus,
       END OF ty_trkorr .


  class-methods OUTPUT
    importing
      !IT_TABLE type STANDARD TABLE
      !IT_DESCRIPTION type SLIS_T_FIELDCAT_ALV .
ENDCLASS.



CLASS Z_QTT IMPLEMENTATION.


method GET_ELEM_INFORMATION.
DATA: lt_elem_output TYPE TABLE OF ty_elem,
      lt_fieldcat    TYPE slis_t_fieldcat_alv,
      ls_fieldcat    TYPE slis_fieldcat_alv.

SELECT eltuid,
       mapname
  FROM rszeltdir
  INTO TABLE @lt_elem_output
  WHERE eltuid IN @it_elem AND
        objvers = @rs_c_objvers-active.

*Create field catalog
ls_fieldcat-fieldname = 'ELTUID'.
ls_fieldcat-seltext_m = 'Element'.
APPEND ls_fieldcat TO lt_fieldcat.

ls_fieldcat-fieldname = 'MAPNAME'.
ls_fieldcat-seltext_m = 'Name'.
APPEND ls_fieldcat TO lt_fieldcat.

IF lt_elem_output[] IS NOT INITIAL.
  output( EXPORTING it_table       = lt_elem_output
                    it_description = lt_fieldcat ).
ENDIF.
endmethod.


method GET_QUERY_ON_REQUESTS.

TYPES: BEGIN OF ty_request,
         trkorr   TYPE trkorr,
         trstatus TYPE trstatus,
         as4user  TYPE tr_as4user,
         as4date  TYPE as4date,
         as4time  TYPE as4time,
         as4text  TYPE as4text,
       END OF ty_request,

       BEGIN OF ty_query,
        compid TYPE sysuuid_25,
       END OF ty_query.

DATA: lt_query      TYPE TABLE OF ty_query,
      lt_tr_desc    TYPE TABLE OF ty_request,
      lt_request    TYPE TABLE OF ty_request,
      lt_transporte TYPE TABLE OF trkorr,
      lt_fieldcat   TYPE slis_t_fieldcat_alv,
      ls_fieldcat   TYPE slis_fieldcat_alv.

"Select all Transport request from E070
SELECT e070~trkorr
       e070~trstatus
       e070~as4user
       e070~as4date
       e070~as4time
       e07t~as4text
  INTO TABLE lt_tr_desc
  FROM e070
  INNER JOIN e07t ON e070~trkorr = e07t~trkorr
  WHERE e070~strkorr = ''.

"Select ID from Query
SELECT eltuid
  FROM rszeltdir
  INTO TABLE lt_query
  WHERE mapname = i_query AND
        objvers = rs_c_objvers-active.

IF lt_query[] IS NOT INITIAL.
  ASSIGN lt_query[ 1 ] TO FIELD-SYMBOL(<ls_query>).
  IF sy-subrc EQ 0.
    TRY.
      <ls_query> = lt_query[ 1 ].
      SELECT trkorr
        INTO TABLE lt_transporte
        FROM e071
        WHERE object = 'ELEM' AND
              obj_name = <ls_query>.

      ASSIGN lt_tr_desc[ 1 ] TO FIELD-SYMBOL(<ls_tr_desc>).
      IF sy-subrc EQ 0.
        LOOP AT lt_transporte ASSIGNING FIELD-SYMBOL(<ls_transporte>).
          TRY.
            <ls_tr_desc> = lt_tr_desc[ trkorr = <ls_transporte> ].
            APPEND <ls_tr_desc> TO lt_request.
          CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDLOOP.
      ENDIF.
    CATCH cx_sy_itab_line_not_found.
      RETURN.
    ENDTRY.
  ENDIF.
ENDIF.

IF lt_request[] IS NOT INITIAL.

*Create field catalog
  ls_fieldcat-fieldname = 'TRKORR'.
  ls_fieldcat-seltext_m = 'Transport Request'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'TRSTATUS'.
  ls_fieldcat-seltext_m = 'Status'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'AS4USER'.
  ls_fieldcat-seltext_m = 'User'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'AS4DATE'.
  ls_fieldcat-seltext_m = 'Date'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'AS4TIME'.
  ls_fieldcat-seltext_m = 'Time'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'AS4TEXT'.
  ls_fieldcat-seltext_m = 'Text'.
  APPEND ls_fieldcat TO lt_fieldcat.

  SORT lt_request ASCENDING BY trkorr.
  output( EXPORTING it_table       = lt_request
                    it_description = lt_fieldcat ).
ENDIF.

endmethod.


method GET_REQUEST_ELEM_CONTENT.
TYPES: BEGIN OF ty_e071,
        trkorr   TYPE trkorr,
        obj_name TYPE trobj_name,
       END OF ty_e071.

DATA: lt_transport_request TYPE TABLE OF ty_trkorr,
      e070                 TYPE TABLE OF ty_trkorr,
      lt_elem              TYPE TABLE OF ty_elem,
      lt_elem_output       TYPE TABLE OF ty_elem,
      lt_e071              TYPE TABLE OF ty_e071,
      lt_fieldcat          TYPE slis_t_fieldcat_alv,
      ls_fieldcat          TYPE slis_fieldcat_alv.

SELECT trkorr,
       trstatus
  FROM e070
  INTO TABLE @lt_transport_request
  WHERE trkorr IN @it_request.

    SELECT trkorr
      FROM e070
      INTO TABLE @e070
      WHERE strkorr IN @it_request.


    SELECT trkorr,
         obj_name
    FROM e071
    INTO TABLE @lt_e071
    WHERE trkorr IN @it_request.

      SELECT eltuid,
             mapname,
             defaulthint
        FROM rszeltdir
        INTO TABLE @lt_elem
        WHERE objvers = @rs_c_objvers-active.

      SELECT trkorr,
             obj_name
        FROM e071
        INTO TABLE @lt_e071
        WHERE object = 'ELEM'.

IF lt_transport_request[] IS NOT INITIAL.
  LOOP AT lt_transport_request ASSIGNING FIELD-SYMBOL(<ls_tr>).
   IF lt_e071 IS INITIAL OR <ls_tr>-trstatus = 'R'.
    IF e070[] IS NOT INITIAL.
      LOOP AT lt_e071 ASSIGNING FIELD-SYMBOL(<ls_e071>).
        READ TABLE e070 ASSIGNING FIELD-SYMBOL(<ls_e070>) WITH KEY trkorr = <ls_e071>-trkorr.
        IF sy-subrc = 0.
          READ TABLE lt_elem ASSIGNING FIELD-SYMBOL(<ls_elem>) WITH KEY eltuid = <ls_e071>-obj_name.
          IF sy-subrc = 0.
            APPEND <ls_elem> TO lt_elem_output.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
   ENDIF.
  ENDLOOP.
ENDIF.

IF lt_elem_output[] IS NOT INITIAL.

*Create field catalog
  ls_fieldcat-fieldname = 'ELTUID'.
  ls_fieldcat-seltext_m = 'Element'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'MAPNAME'.
  ls_fieldcat-seltext_m = 'Name'.
  APPEND ls_fieldcat TO lt_fieldcat.

  ls_fieldcat-fieldname = 'DEFAULTHINT'.
  ls_fieldcat-seltext_m = 'Object'.
  APPEND ls_fieldcat TO lt_fieldcat.

  output( Exporting it_table       = lt_elem_output
                    it_description = lt_fieldcat ).

ENDIF.

endmethod.


METHOD output.
DATA: gr_table  TYPE REF TO cl_salv_table,
      dref      TYPE REF TO data,
      lv_layout TYPE slis_layout_alv.

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

lv_layout-colwidth_optimize  = rs_c_true.

CREATE DATA dref LIKE it_table.
ASSIGN dref->* TO <fs_table>.

<fs_table> = it_table.

TRY.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     is_layout   = lv_layout
     it_fieldcat = it_description
   TABLES
     t_outtab    = <fs_table>.

CATCH cx_salv_not_found.
CATCH cx_salv_msg.
ENDTRY.
ENDMETHOD.
ENDCLASS.
