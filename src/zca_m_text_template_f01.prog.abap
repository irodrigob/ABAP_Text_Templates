*&---------------------------------------------------------------------*
*& Include          ZCA_M_TEXT_TEMPLATE_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form CHANGE_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM change_template .
  DATA lv_respuesta TYPE c.
  " Se valida si la applicacion y template existe.
  IF mo_controller->exist( iv_appl = mv_appl iv_template = mv_template ) = abap_true.

    " Por defecto el modo de edición es editar
    mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-edit.
    PERFORM read_template.

  ELSE. " No existe
    " Se pregunta si se quiere crear el objeto
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = TEXT-t01
        text_question  = TEXT-t02
        text_button_1  = TEXT-yes
        text_button_2  = TEXT-nop
        default_button = '2'
      IMPORTING
        answer         = lv_respuesta
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    CASE lv_respuesta.
      WHEN zif_ca_ttemplate_data=>cs_edit_program-popup_answer-yes.
        mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-create.

        " Se limpian las variables individuales de cada sección
        mo_controller->clear_section_single_var(  ).

      WHEN OTHERS.

        " Se limpian las variables introducidas
        CLEAR: mv_appl, mv_template.

    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM read_template .

  mo_controller->read( ).

  " Se propoga los valores de las secciones a las variables de cada pantalla
  mo_controller->propagate_sections_data(  ).

ENDFORM.

FORM save_template.
  TRY.
      mo_controller->save( ).

      MESSAGE s002.

    CATCH zcx_ca_text_template INTO DATA(lo_excep).
      MESSAGE lo_excep->get_text(  ) TYPE 'W'.
  ENDTRY.
ENDFORM.
FORM copy_template.
  DATA lt_fields TYPE STANDARD TABLE OF sval.
  DATA lv_code TYPE c.

  " Inserto los campos donde se introducirán el nombre y aplicacion, por defecto se informa la aplicación.
  lt_fields = VALUE #( ( tabname = |ZCA_T_TEXT_TEMPL| fieldname = |APPL| value = mv_appl )
                       ( tabname = |ZCA_T_TEXT_TEMPL| fieldname = |NAME| ) ).

  DO.
    CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
      EXPORTING
        popup_title     = TEXT-t03
        f4_formname     = 'F4_TEMPLATE'
        f4_programname  = sy-cprog
      IMPORTING
        returncode      = lv_code
      TABLES
        fields          = lt_fields[]
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF lv_code IS INITIAL. " Enter

      " Se recupera los valores introducidos
      READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>) INDEX 1.
      DATA(lv_new_appl) = <ls_fields>-value.
      READ TABLE lt_fields ASSIGNING <ls_fields> INDEX 2.
      DATA(lv_new_template) = <ls_fields>-value.

      " Los dos campos han de estar informados.
      IF lv_new_appl IS NOT INITIAL AND lv_new_template IS NOT INITIAL.

        IF mo_controller->exist( iv_appl = CONV #( lv_new_appl ) iv_template = CONV #( lv_new_template ) ) = abap_true.
          MESSAGE s004.
        ELSE.

          EXIT.
        ENDIF.
      ELSE.
        MESSAGE s004.
      ENDIF.

    ELSE. " Boton cancelar que devuelve una 'A'.
      CLEAR: lv_new_template, lv_new_appl.
      EXIT.
    ENDIF.
  ENDDO.

  " Si los dos campos están informados se hace la copia
  IF lv_new_appl IS NOT INITIAL AND lv_new_template IS NOT INITIAL.

    TRY.
        mo_controller->copy( iv_appl_from = mv_appl
                             iv_appl_to = CONV #( lv_new_appl )
                             iv_template_from = mv_template
                             iv_template_to = CONV #( lv_new_template ) ).
        MESSAGE s006.
      CATCH zcx_ca_text_template INTO DATA(lo_excep).
        MESSAGE s000 WITH lo_excep->get_text(  ).
    ENDTRY.
  ENDIF.


ENDFORM.
FORM delete_template.

  TRY.
      mo_controller->delete( ).

      MESSAGE s001.

    CATCH zcx_ca_text_template INTO DATA(lo_excep).
      MESSAGE lo_excep->get_text(  ) TYPE 'W'.
  ENDTRY.


ENDFORM.
