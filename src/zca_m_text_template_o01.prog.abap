*&---------------------------------------------------------------------*
*& Include          ZCA_M_TEXT_TEMPLATE_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'D9000'.
  SET TITLEBAR 'T9000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9000 OUTPUT.

  LOOP AT SCREEN.


    " Si hay modo de edición es que se ha introducido un template valido por lo tanto los campos se dejan como lectura y se habilitan los campos
    IF mv_edit_mode IS NOT INITIAL.
      CASE screen-group1.
          " Campos que el grupo de template seleccionado
        WHEN zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_select.
          screen-active = '1'.

          " Para los campos que son de edición hay que ver el modo de edición para ver si hay que ocultarlos o no.
          CASE screen-group2.
            WHEN zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_edit.
              IF mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-display.
                screen-active = 0.

              ELSE.
                screen-active = 1.

                " Para aquellos campos de borrado solo aparecen cuando se este editando
                IF screen-group3 = zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_delete AND
                   mv_edit_mode NE zif_ca_ttemplate_data=>cs_edit_program-edit_mode-edit.
                  screen-active = 0.
                ENDIF.
              ENDIF.
          ENDCASE.

          " Campos cuando el template no esta informado
        WHEN  zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_empty.
          screen-input = '0'.
      ENDCASE.
    ELSE.
      CASE screen-group1.
          " Campos cuando el template no esta informado
        WHEN  zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_empty.
          screen-input = 1.
        WHEN zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_select.
          screen-active = 0.
      ENDCASE.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CONTROLS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls OUTPUT.

  IF mo_controller IS NOT BOUND.
    mo_controller = NEW lcl_contr(  ).
  ENDIF.

  " El idioma por defecto será el de logon
  mv_langu = COND #( WHEN mv_langu IS INITIAL THEN sy-langu ELSE mv_langu ).

  " Se guarda el último idioma usado para poder restaurarlo en caso necesario cuando se cambie de idioma
  mv_langu_last = mv_langu.

  " Se guarda el último asunto introducido
  mv_mail_subject_last = mv_mail_subject.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_SECTIONS'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab_sections_active_tab_set OUTPUT.
  tab_sections-activetab = g_tab_sections-pressed_tab.
  CASE g_tab_sections-pressed_tab.
    WHEN c_tab_sections-tab1.
      g_tab_sections-subscreen = '9001'.
    WHEN c_tab_sections-tab2.
      g_tab_sections-subscreen = '9002'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9001 OUTPUT.
  LOOP AT SCREEN.
    " Solo para campos de la dynpro de mail
    IF screen-group4 = zif_ca_ttemplate_data=>cs_edit_program-screen_group-dynpro_mail.
      IF mv_edit_mode IS INITIAL. " Sin modo edición todos los campos se ocultan
        screen-active = 0.
      ELSE.
        " En modo visualización los campos de edicion se dejan sin entrada.s
        CASE screen-group2.
          WHEN zif_ca_ttemplate_data=>cs_edit_program-screen_group-template_edit.
            IF mv_edit_mode = zif_ca_ttemplate_data=>cs_edit_program-edit_mode-display.
              screen-input = 0.

            ELSE.
              screen-input = 1.

            ENDIF.

        ENDCASE.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CONTROLS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE controls_9001 OUTPUT.
  TRY.
      " Se inicializa el editor para el cuerpo del mensaje
      mo_controller->editor_mail_object(  ).

    CATCH zcx_ca_text_template INTO DATA(lx_excep).
      MESSAGE lx_excep->get_text( ) TYPE 'E'.
  ENDTRY.

ENDMODULE.
