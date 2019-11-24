*&---------------------------------------------------------------------*
*& Include ZCA_R_TEXT_TEMPLATE_TOP                  - Module Pool      ZCA_R_TEXT_TEMPLATE
*&---------------------------------------------------------------------*
PROGRAM zca_r_text_template.

*----------------------------------------------------------------------*
* Variables de dynpro 9000
*----------------------------------------------------------------------*
* Variables para saber la aplicación, template e idioma
DATA mv_appl TYPE zca_t_text_templ-appl.
DATA mv_appl_last TYPE zca_t_text_templ-appl.
DATA mv_template TYPE zca_t_text_templ-name.
DATA mv_template_last TYPE zca_t_text_templ-name.
DATA mv_langu TYPE zca_t_text_templ-langu.
DATA mv_langu_last TYPE zca_t_text_templ-langu.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_SECTIONS'
CONSTANTS: BEGIN OF c_tab_sections,
             tab1 LIKE sy-ucomm VALUE 'TAB_SECTIONS_MAIL',
             tab2 LIKE sy-ucomm VALUE 'TAB_SECTIONS_OTHER',
           END OF c_tab_sections.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_SECTIONS'
CONTROLS:  tab_sections TYPE TABSTRIP.
DATA: BEGIN OF g_tab_sections,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZCA_M_TEXT_TEMPLATE',
        pressed_tab LIKE sy-ucomm VALUE c_tab_sections-tab1,
      END OF g_tab_sections.
DATA:      ok_code LIKE sy-ucomm.


*----------------------------------------------------------------------*
* Variables de dynpro 9001
*----------------------------------------------------------------------*
* Variables de para guardar el cuerpo y asunto del mail
DATA mv_mail_body TYPE string.
DATA mv_mail_subject TYPE string.

*----------------------------------------------------------------------*
* Variables generales
*----------------------------------------------------------------------*

CLASS lcl_contr DEFINITION DEFERRED.
DATA mo_controller TYPE REF TO lcl_contr.


* Variable que controla el modo de edición
DATA mv_edit_mode TYPE zif_ca_ttemplate_data=>tv_edit.
