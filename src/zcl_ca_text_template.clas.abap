CLASS zcl_ca_text_template DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_section_content,
        langu   TYPE sylangu,
        section TYPE zca_e_ttempl_section,
        content TYPE zca_e_ttempl_content,
      END OF ts_section_content .
    TYPES:
      tt_section_content TYPE STANDARD TABLE OF ts_section_content WITH EMPTY KEY .
    TYPES: BEGIN OF ts_key_fields,
             appl     TYPE zca_e_ttempl_appl,
             name     TYPE zca_e_ttempl_name,
             langu    TYPE sylangu,
             tsection TYPE zca_e_ttempl_section,
           END OF ts_key_fields.
    TYPES: tt_key_fields TYPE STANDARD TABLE OF ts_key_fields WITH EMPTY KEY.

    METHODS exist
      IMPORTING
        !iv_appl        TYPE zca_e_ttempl_appl
        !iv_name        TYPE zca_e_ttempl_name
        !iv_langu       TYPE sylangu OPTIONAL
      RETURNING
        VALUE(rv_exist) TYPE sap_bool .
    METHODS read
      IMPORTING
        !iv_appl  TYPE zca_e_ttempl_appl
        !iv_name  TYPE zca_e_ttempl_name
        !iv_langu TYPE sylangu OPTIONAL
      EXPORTING
        !et_data  TYPE tt_section_content .
    METHODS save
      IMPORTING
                !iv_appl              TYPE zca_e_ttempl_appl
                !iv_name              TYPE zca_e_ttempl_name
                !it_data              TYPE tt_section_content
                !iv_save_transp_order TYPE sap_bool DEFAULT abap_true
      CHANGING  cv_order              TYPE e070-trkorr OPTIONAL
      RAISING
                zcx_ca_text_template .
    METHODS delete
      IMPORTING
        !iv_appl  TYPE zca_e_ttempl_appl
        !iv_name  TYPE zca_e_ttempl_name
        !iv_langu TYPE sylangu OPTIONAL.
    METHODS copy
      IMPORTING
                iv_appl_from TYPE zca_e_ttempl_appl
                iv_appl_to   TYPE zca_e_ttempl_appl
                iv_name_from TYPE zca_e_ttempl_name
                iv_name_to   TYPE zca_e_ttempl_name
                iv_langu     TYPE sylangu OPTIONAL
      RAISING   zcx_ca_text_template.
    "! <p class="shorttext synchronized" lang="en">Save the template in transport order</p>
    "!
    "! @parameter it_keys | <p class="shorttext synchronized" lang="en">Values of fields keys of template</p>
    METHODS save_transport_order
      IMPORTING
                !it_keys    TYPE zcl_ca_text_template=>tt_key_fields
                !iv_objfunc TYPE objfunc DEFAULT zif_ca_ttemplate_data=>cs_transport_order-order_objfunc-key_value
      CHANGING
                !cv_order   TYPE e070-trkorr
      RAISING   zcx_ca_text_template.
    "! <p class="shorttext synchronized" lang="en">Check if the transport order is valid</p>
    "!
    "! @parameter cv_order | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_ca_text_template | <p class="shorttext synchronized" lang="en"></p>
    METHODS check_transport_order
      CHANGING cv_order TYPE e070-trkorr
      RAISING  zcx_ca_text_template.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_text_template IMPLEMENTATION.


  METHOD copy.

    " Primero se lee la plantilla de origen
    read( EXPORTING iv_appl  = iv_appl_from
                    iv_name  = iv_name_from
                    iv_langu = iv_langu
                    IMPORTING et_data  = DATA(lt_data_from) ).

    IF lt_data_from IS NOT INITIAL. " Si hay datos se graba con el nuevo nombre
      TRY.
          save( EXPORTING iv_appl = iv_appl_to
                          iv_name              = iv_name_to
                          it_data              = lt_data_from ).
        CATCH zcx_ca_text_template.
          RAISE EXCEPTION TYPE zcx_ca_text_template
            EXPORTING
              textid = zcx_ca_text_template=>error_to_copy.
      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>template_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD delete.
    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    DELETE FROM zca_t_text_templ WHERE appl = iv_appl
                                       AND name = iv_name
                                       AND langu IN lt_r_langu.
  ENDMETHOD.


  METHOD exist.

    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    SELECT SINGLE @abap_true INTO @rv_exist
           FROM zca_t_text_templ
           WHERE appl = @iv_appl
                 AND name = @iv_name
                 AND langu IN @lt_r_langu.


  ENDMETHOD.


  METHOD read.

    CLEAR et_data.

    IF iv_langu IS NOT INITIAL.
      DATA(lt_r_langu) = VALUE zif_ca_ttemplate_data=>tt_r_langu( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    SELECT langu, tsection AS section, content  INTO TABLE @et_data
           FROM zca_t_text_templ
           WHERE appl = @iv_appl
                 AND name = @iv_name
                 AND langu IN @lt_r_langu.

  ENDMETHOD.


  METHOD save.
    DATA lt_data_db TYPE STANDARD TABLE OF zca_t_text_templ.

    IF it_data IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>no_data_to_save.
    ENDIF.

    lt_data_db = VALUE #( FOR <wa> IN it_data ( appl = iv_appl name = iv_name
                                                langu = <wa>-langu
                                                tsection = <wa>-section
                                                content = <wa>-content )  ).

    MODIFY zca_t_text_templ FROM TABLE lt_data_db.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.

      " Si guarda el contenido en una orden
      IF iv_save_transp_order = abap_true.
        " Se rellenan la clave a transportar
        DATA(lt_keys) = CORRESPONDING tt_key_fields( lt_data_db ).
        save_transport_order( EXPORTING it_keys = lt_keys
                              CHANGING cv_order = cv_order ).
      ENDIF.

    ELSE.

      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>error_to_save.

    ENDIF.
  ENDMETHOD.

  METHOD save_transport_order.
    DATA lt_e071k TYPE STANDARD TABLE OF e071k.
    DATA lt_e071 TYPE STANDARD TABLE OF e071.

    " Se chequea que la orden sea valida
    check_transport_order( CHANGING cv_order = cv_order ).

    " Dato de la cabecera
    APPEND VALUE #( pgmid = zif_ca_ttemplate_data=>cs_transport_order-pgmid
                    object = zif_ca_ttemplate_data=>cs_transport_order-object
                    obj_name = zif_ca_ttemplate_data=>cs_transport_order-tabname
                    objfunc = iv_objfunc ) TO lt_e071.

* Se Añade las entradas de los campos clave
    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
      APPEND VALUE #( pgmid = zif_ca_ttemplate_data=>cs_transport_order-pgmid
                    object = zif_ca_ttemplate_data=>cs_transport_order-object
                    objname = zif_ca_ttemplate_data=>cs_transport_order-tabname
                    mastertype = zif_ca_ttemplate_data=>cs_transport_order-object
                    tabkey = <ls_key>  ) TO lt_e071k.
    ENDLOOP.

    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
        wi_simulation         = ' '
        wi_suppress_key_check = ' '
        wi_trkorr             = cv_order
      TABLES
        wt_e071               = lt_e071
        wt_e071k              = lt_e071k
      EXCEPTIONS
        OTHERS                = 68.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD check_transport_order.
    DATA lt_req_head TYPE trwbo_request_headers.
    DATA lt_req TYPE trwbo_requests.

    IF cv_order IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_text_template
        EXPORTING
          textid = zcx_ca_text_template=>transport_order_mandatory.
    ENDIF.

    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
      EXPORTING
        iv_trkorr          = cv_order
      IMPORTING
        et_request_headers = lt_req_head
        et_requests        = lt_req
      EXCEPTIONS
        invalid_input      = 1
        OTHERS             = 2.

    IF sy-subrc NE 0. " Si no existe se lanza excepción
      RAISE EXCEPTION TYPE zcx_ca_text_template
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Se busca una tarea valida en la orden
    READ TABLE lt_req_head ASSIGNING FIELD-SYMBOL(<ls_req_head>) WITH KEY trfunction = zif_ca_ttemplate_data=>cs_transport_order-workbench
                                                                            trstatus = zif_ca_ttemplate_data=>cs_transport_order-status_modifiable
                                                                            as4user = sy-uname.
    IF sy-subrc = 0.
      cv_order = <ls_req_head>-trkorr.
    ELSE.
      " Si no la tiene, se lee el registro de la orden padre para poder crear una tarea a la orden
      LOOP AT lt_req_head ASSIGNING <ls_req_head> WHERE strkorr IS INITIAL.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.

        CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
          EXPORTING
            wi_kurztext       = <ls_req_head>-as4text
            wi_trfunction     = zif_ca_ttemplate_data=>cs_transport_order-workbench
            iv_username       = sy-uname
            wi_strkorr        = <ls_req_head>-strkorr
            wi_client         = sy-mandt
          IMPORTING
            we_trkorr         = cv_order
          EXCEPTIONS
            no_systemname     = 1
            no_systemtype     = 2
            no_authorization  = 3
            db_access_error   = 4
            file_access_error = 5
            enqueue_error     = 6
            number_range_full = 7
            invalid_input     = 8
            OTHERS            = 9.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_ca_text_template
            MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
