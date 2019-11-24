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

          MESSAGE TEXT-s01 TYPE 'S'.
        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_display.
          mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-display.
          PERFORM read_template.
        WHEN zif_ca_ttemplate_data=>cs_edit_program-buttons_code-template_edit.
          mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-display.
          PERFORM read_template.
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
