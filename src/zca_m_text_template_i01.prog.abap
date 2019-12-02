*&---------------------------------------------------------------------*
*& Include          ZCA_M_TEXT_TEMPLATE_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_PROGRAM  INPUT
*&---------------------------------------------------------------------*
MODULE exit_program INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_TEMPLATE  INPUT
*&---------------------------------------------------------------------*
MODULE change_template INPUT.

  PERFORM change_template.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  TRY.
      CASE sy-ucomm.
        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_close.
          mo_controller->close_template(  ).
        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_save.

          mo_controller->save( ).
          MESSAGE s002. " Mensaje que ha ido bien si no hay excepción

        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_display.

          mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-display.
          PERFORM read_template.

        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_edit.

          mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-edit.
          PERFORM read_template.

        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_delete.

          mo_controller->delete( ).
          MESSAGE s001. " Mensaje que ha ido bien si no hay excepción

        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_copy.

          PERFORM copy_template.

        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_transport.

          mo_controller->transport( ).
          MESSAGE s016. " Mensaje que ha ido bien si no hay excepción

      ENDCASE.
    CATCH zcx_ca_text_template INTO DATA(lo_excep_user_command).

      MESSAGE lo_excep_user_command->get_text(  ) TYPE 'W'.

  ENDTRY.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_SECTIONS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab_sections_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_sections-tab1.
      g_tab_sections-pressed_tab = c_tab_sections-tab1.
    WHEN c_tab_sections-tab2.
      g_tab_sections-pressed_tab = c_tab_sections-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_LAGUAGE  INPUT
*&---------------------------------------------------------------------*
MODULE change_laguage INPUT.
  DATA lv_answer TYPE c.
  " Solo hay cambio de idioma cuando el introducido y el anterior son distintos
  IF mv_langu NE mv_langu_last.

    " Se mira si ha habido cambios en los campos de mail
    IF mo_controller->check_mail_changed(  ) = abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = TEXT-t04
          text_question  = TEXT-t05
          text_button_1  = TEXT-yes
          text_button_2  = TEXT-nop
          default_button = '2'
        IMPORTING
          answer         = lv_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.

      CASE lv_answer.
        WHEN zif_ca_ttemplate_data=>cs_edit_program-popup_answer-yes.
          TRY.
              " Se graba en el idioma anterior
              mo_controller->save( mv_langu_last ).

              " Se lee la nueva plantilla en el idioma
              PERFORM read_template.

              MESSAGE s002.
            CATCH zcx_ca_text_template.
              MESSAGE w003.
          ENDTRY.

        WHEN OTHERS. " Cualquier otra cosa se restaura el idioma orignal

          mv_langu = mv_langu_last.

      ENDCASE.
    ELSE. " Si no hay cambios se lee la plantilla
      PERFORM read_template.
    ENDIF.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_APPL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_appl INPUT.
  mo_controller->f4_appl( EXPORTING iv_dynpro = sy-dynnr
                                    iv_program = sy-repid ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_TEMPLATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_template INPUT.
  mo_controller->f4_template( EXPORTING iv_dynpro = sy-dynnr
                                      iv_program = sy-repid ).


  " Si se ha seleccionado aplicacion y template se simula que se pulsa enter(no hay manera
  " de hacerlo de otra forma) para que la plantilla se vea en modo edición
  IF mv_appl IS NOT INITIAL AND mv_template IS NOT INITIAL.

    PERFORM change_template.
    SET SCREEN 9000. LEAVE SCREEN.

  ENDIF.
ENDMODULE.
