*&---------------------------------------------------------------------*
*& Include          ZCA_M_TEXT_TEMPLATE_C01
*&---------------------------------------------------------------------*
CLASS lcl_contr DEFINITION.

  PUBLIC SECTION.


    METHODS constructor.
    METHODS exist
      IMPORTING
                !iv_appl        TYPE zca_e_ttempl_appl
                !iv_template    TYPE zca_e_ttempl_name
      RETURNING VALUE(rv_exist) TYPE sap_bool.

    METHODS read.

    METHODS clear_var.
    METHODS clear_section_single_var.
    METHODS propagate_sections_data.
    METHODS editor_mail_object
      RAISING zcx_ca_text_template.
    METHODS close_template.
    METHODS set_content_mail_body.
    METHODS save
      IMPORTING
                !iv_langu TYPE sylangu OPTIONAL
      RAISING   zcx_ca_text_template.
    METHODS delete.
    METHODS copy
      IMPORTING
                iv_appl_from     TYPE zca_t_text_templ-appl
                iv_appl_to       TYPE zca_t_text_templ-appl
                iv_template_from TYPE zca_t_text_templ-name
                iv_template_to   TYPE zca_t_text_templ-name
      RAISING   zcx_ca_text_template.
    METHODS edit_mail_created
      RETURNING VALUE(rv_created) TYPE sap_bool.
    METHODS check_mail_changed
      RETURNING
        VALUE(rv_changed) TYPE sap_bool.

  PROTECTED SECTION.
    " Tabla interna que contiene todos las secciones en todos los idiomas de la plantilla
    DATA mt_sections_data TYPE zcl_ca_text_template=>tt_section_content.

    " Clase que gestiona todas las operaciones con el modelo de datos
    DATA mo_model TYPE REF TO zcl_ca_text_template.

    " Variables para hacer el editor HTML para el cuerpo del mail
    DATA mo_mail_body_cnt   TYPE REF TO cl_gui_custom_container.
    DATA mo_mail_body_editor   TYPE REF TO if_btf_editor.
    DATA mo_mail_body_doc TYPE REF TO if_btf_document.
    DATA mv_transf_mail_body_editor TYPE sap_bool.
    METHODS destroy_mail_body_object.
    METHODS create_editor_mail_objects
      RAISING zcx_ca_text_template.
    METHODS transf_var_mail_2_sections
      IMPORTING !iv_langu TYPE sylangu OPTIONAL.
    METHODS transf_var_mail_2_sect_mail
      IMPORTING !iv_langu TYPE sylangu OPTIONAL.
    METHODS get_content_mail_body
      RETURNING
        VALUE(rv_body) TYPE bsstring.




ENDCLASS.

CLASS lcl_contr IMPLEMENTATION.

  METHOD constructor.
    mo_model = NEW zcl_ca_text_template(  ).
  ENDMETHOD.

  METHOD exist.
    rv_exist = mo_model->exist( iv_appl = iv_appl iv_name = iv_template ).
  ENDMETHOD.

  METHOD read.

    clear_section_single_var(  ). " Se resetean variables de las seccion

    mv_transf_mail_body_editor = abap_false. " Se indica que en cuerpo no se ha transferido para que se haga al leer los datos

    " Se leen los datos del modelo
    mo_model->read(
      EXPORTING
        iv_appl  = mv_appl
        iv_name  = mv_template
       iv_langu = mv_langu
      IMPORTING
        et_data  = mt_sections_data ).

  ENDMETHOD.

  METHOD clear_var.
    CLEAR: mv_appl, mv_template, mv_langu, mt_sections_data, mv_edit_mode.

    mv_transf_mail_body_editor = abap_false.

    " Las variables individuales según cada seccion.
    clear_section_single_var(  ).
  ENDMETHOD.

  METHOD clear_section_single_var.
    " Variables del mail
    CLEAR: mv_mail_body, mv_mail_subject.
  ENDMETHOD.


  METHOD propagate_sections_data.
    " Limpia las variables individuales donde se guardan los valores de las secciones.
    clear_section_single_var(  ).

    " Se recorre la tabla de datos asignando cada sección a una variable global.
    LOOP AT mo_controller->mt_sections_data ASSIGNING FIELD-SYMBOL(<ls_sections_data>) WHERE langu = mv_langu.


      CASE <ls_sections_data>-section.
        WHEN zif_ca_ttemplate_data=>cs_section-body.
          mv_mail_body = <ls_sections_data>-content.
        WHEN zif_ca_ttemplate_data=>cs_section-subject.
          mv_mail_subject = <ls_sections_data>-content.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD editor_mail_object.

    " Se crea los objetos para poder mostrar el editor
    create_editor_mail_objects( ).

    " Se transfiere el cuerpo del mensaje al editor
    set_content_mail_body(  ).

  ENDMETHOD.

  METHOD close_template.
    " Se limpian las variables
    clear_var(  ).

    " Se destruye el editor del cuerpo del mail
    destroy_mail_body_object( ).
  ENDMETHOD.


  METHOD destroy_mail_body_object.
    IF mo_mail_body_editor IS BOUND.
      mo_mail_body_editor->free( ).
    ENDIF.

    IF mo_mail_body_cnt IS NOT INITIAL.
      CALL METHOD mo_mail_body_cnt->free
        EXCEPTIONS
          OTHERS = 1.
    ENDIF.

    FREE: mo_mail_body_cnt, mo_mail_body_editor, mo_mail_body_doc.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.


  METHOD create_editor_mail_objects.
    " Si el container ya ha sido creado y se esta editando es cuando se crea el objeto del editor
    IF mo_mail_body_cnt IS NOT BOUND AND mv_edit_mode IS NOT INITIAL.

      CREATE OBJECT mo_mail_body_cnt
        EXPORTING
          container_name = zif_ca_ttemplate_data=>cs_edit_program-containers_dynpro-mail_editor
        EXCEPTIONS
          OTHERS         = 1.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_ca_text_template
          EXPORTING
            textid = zcx_ca_text_template=>error_create_mail_editor.
      ENDIF.

      TRY.
          " Se crea el documento
          mo_mail_body_doc = cl_btf=>get_reference( )->create_document( mv_langu ).

          " Se crea el editor en base al documento
          mo_mail_body_editor = cl_btf=>get_reference( )->create_editor( mo_mail_body_doc ).

          " Se asocia el container de la dynpro al editor
          CALL METHOD mo_mail_body_editor->initialize
            EXPORTING
              ctrl_parent = mo_mail_body_cnt
              design_mode = if_btf_editor_constants=>co_design_mode_on.

          " Opciones del editor
          DATA(lo_btf_editor_options) = CAST if_btf_editor_options( mo_mail_body_editor ).
          lo_btf_editor_options->set_tab_control(      if_btf_editor_options=>co_tab_control_on ).
          lo_btf_editor_options->set_local_operations( if_btf_editor_options=>co_local_operations_on ).

        CATCH cx_root.
          RAISE EXCEPTION TYPE zcx_ca_text_template
            EXPORTING
              textid = zcx_ca_text_template=>error_create_mail_editor.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD set_content_mail_body.
    DATA lo_cond_editor         TYPE REF TO cl_umg_condition_editor.

    " Solo se transfiere si la variable de control esta a falso y el editor esta instanciado
    IF mv_transf_mail_body_editor = abap_false AND mo_mail_body_editor IS BOUND.
      mv_transf_mail_body_editor = abap_true.


      "DUMMY SO WE CAN USE METHOD STRING TO TABLE :D
      CREATE OBJECT lo_cond_editor
        EXPORTING
          parent = cl_gui_container=>screen0. "Dummy!

      CALL METHOD cl_gui_cfw=>flush.
      lo_cond_editor->free( ).
      "END DUMMY
      TRY.
          DATA(lv_xdocument) = cl_bcs_convert=>string_to_xstring( mv_mail_body ).

          DATA(lv_xdocument_length) = xstrlen( lv_xdocument ).

          mo_mail_body_doc->set_content( text     = lv_xdocument
                              encoding = zif_ca_ttemplate_data=>cs_edit_program-btf_editor-encoding ).


          mo_mail_body_editor->set_content( ).

        CATCH cx_root.
          RAISE EXCEPTION TYPE zcx_ca_text_template
            EXPORTING
              textid = zcx_ca_text_template=>error_to_transf_data_to_editor.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD save.
    " Primero es mover las variables del programa a sus respectiva sección
    transf_var_mail_2_sections( iv_langu ).

    mo_model->save(
      EXPORTING
        iv_appl              = mv_appl
        iv_name              = mv_template
        it_data              = mt_sections_data ).

  ENDMETHOD.


  METHOD transf_var_mail_2_sections.

    " Mail
    transf_var_mail_2_sect_mail( iv_langu ).


  ENDMETHOD.


  METHOD transf_var_mail_2_sect_mail.

    " El idioma se toma primero del parámetor en caso contrario del idioma
    IF iv_langu IS NOT INITIAL.
      DATA(lv_langu) = iv_langu.
    ELSE.
      lv_langu = mv_langu.
    ENDIF.

    " Asunto
    READ TABLE mt_sections_data ASSIGNING FIELD-SYMBOL(<ls_section>) WITH KEY section = zif_ca_ttemplate_data=>cs_section-subject
                                                                                          langu = lv_langu.
    IF sy-subrc NE 0.
      INSERT VALUE #( langu = lv_langu section = zif_ca_ttemplate_data=>cs_section-subject content = mv_mail_subject ) INTO TABLE mt_sections_data.
    ELSE.
      <ls_section>-content = mv_mail_subject.
    ENDIF.

    " Cuerpo
    READ TABLE mt_sections_data ASSIGNING <ls_section> WITH KEY section = zif_ca_ttemplate_data=>cs_section-body
                                                                                      langu = lv_langu.
    IF sy-subrc NE 0.
      INSERT VALUE #( langu = lv_langu section = zif_ca_ttemplate_data=>cs_section-body content = get_content_mail_body(  ) ) INTO TABLE mt_sections_data.
    ELSE.
      <ls_section>-content = get_content_mail_body(  ).
    ENDIF.


  ENDMETHOD.


  METHOD get_content_mail_body.
    DATA lv_codepage TYPE cpcodepage.

    CLEAR: rv_body.

    TRY.
        mo_mail_body_editor->get_content( ).

        mo_mail_body_doc->get_content( IMPORTING text     = DATA(lv_xdocument)
                                                encoding = DATA(lv_encoding) ).

        DATA(lv_xdocument_length) = xstrlen( lv_xdocument ).

        CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
          EXPORTING
            external_name = lv_encoding
          IMPORTING
            sap_codepage  = lv_codepage
          EXCEPTIONS
            not_found     = 1
            OTHERS        = 2.

        CALL FUNCTION 'SCP_TRANSLATE_CHARS'
          EXPORTING
            inbuff           = lv_xdocument
            inbufflg         = lv_xdocument_length
            incode           = lv_codepage
            substc_space     = 'X'
            substc           = '00035'
          IMPORTING
            outbuff          = rv_body
          EXCEPTIONS
            invalid_codepage = 1
            internal_error   = 2
            cannot_convert   = 3
            fields_bad_type  = 4
            OTHERS           = 5.


      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD delete.
    mo_model->delete(
      EXPORTING
        iv_appl  = mv_appl
        iv_name  = mv_template ).

  ENDMETHOD.


  METHOD copy.

    mo_model->copy(
      EXPORTING
        iv_appl_from         = iv_appl_from
        iv_appl_to           = iv_appl_to
        iv_name_from         = iv_template_from
        iv_name_to           = iv_template_to ).

  ENDMETHOD.
  METHOD edit_mail_created.
    IF mo_mail_body_editor IS BOUND.
      rv_created = abap_true.
    ELSE.
      rv_created = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD check_mail_changed.
    rv_changed = abap_false. " Por defecto no hay cambios

    " Se mira si el asunto ha cambiado
    IF mv_mail_subject NE mv_mail_subject_last.
      rv_changed = abap_true.
    ELSE. " Se mira el cuerpo
      IF mo_mail_body_editor IS BOUND. " Solo si el editor ha sido instanciado
        IF mo_mail_body_editor->get_is_modified(  ) = 1.
          rv_changed = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
