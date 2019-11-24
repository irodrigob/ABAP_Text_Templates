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
