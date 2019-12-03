INTERFACE zif_ca_ttemplate_data
  PUBLIC .
  TYPES: tv_edit TYPE c LENGTH 1.
  TYPES: tt_r_langu TYPE RANGE OF sylangu.
  TYPES: tt_r_appl TYPE RANGE OF zca_t_text_templ-appl.
  TYPES: tt_r_template TYPE RANGE OF zca_t_text_templ-name.

  CONSTANTS: BEGIN OF cs_section,
               subject TYPE zca_e_ttempl_section VALUE 'S',
               body    TYPE zca_e_ttempl_section VALUE 'B',
             END OF cs_section.
  CONSTANTS: cv_main_tabname TYPE tabname VALUE 'ZCA_T_TEXT_TEMPL'.

  CONSTANTS: BEGIN OF cs_edit_program,
               BEGIN OF edit_mode,
                 display TYPE tv_edit VALUE 'D',
                 edit    TYPE tv_edit VALUE 'E',
                 create  TYPE tv_edit VALUE 'C',
               END OF edit_mode,
               BEGIN OF screen_group,
                 template_select TYPE string VALUE 'TML',
                 template_edit   TYPE string VALUE 'EDI',
                 template_empty  TYPE string VALUE 'TEM',
                 template_delete TYPE string VALUE 'DEL',
                 dynpro_mail     TYPE string VALUE 'MAI',
               END OF screen_group,
               BEGIN OF buttons_code,
                 template_create    TYPE string VALUE 'TCREAT',
                 template_display   TYPE string VALUE 'TVIEW',
                 template_edit      TYPE string VALUE 'TEDIT',
                 template_delete    TYPE string VALUE 'TDELE',
                 template_transport TYPE string VALUE 'TTRANSP',
                 template_save      TYPE string VALUE 'TSAVE',
                 template_close     TYPE string VALUE 'TCLOSE',
                 template_copy      TYPE string VALUE 'TCOPY',
               END OF buttons_code,
               BEGIN OF popup_answer,
                 yes    TYPE c LENGTH 1 VALUE '1',
                 no     TYPE c LENGTH 1 VALUE '2',
                 cancel TYPE c LENGTH 1 VALUE 'A',
               END OF popup_answer,
               BEGIN OF containers_dynpro,
                 mail_editor TYPE c LENGTH 60 VALUE 'CNT_BODY_MAIL',
               END OF containers_dynpro,
               BEGIN OF btf_editor,
                 encoding TYPE string VALUE 'utf-8',
               END OF btf_editor,
             END OF cs_edit_program.
  CONSTANTS: BEGIN OF cs_transport_order,
               workbench         TYPE e070-trfunction VALUE 'S',
               status_modifiable TYPE trstatus VALUE 'D',
               pgmid             TYPE e071k-pgmid VALUE 'R3TR',
               object            TYPE e071k-object VALUE 'TABU',
               tabname           TYPE e071k-objname VALUE 'ZCA_T_TEXT_TEMPL',
               category          TYPE e070-korrdev VALUE 'SYST',
               BEGIN OF order_objfunc,
                 normal    TYPE objfunc VALUE space,
                 delete    TYPE objfunc VALUE 'D',
                 key_value TYPE objfunc VALUE 'K',
               END OF order_objfunc,
             END OF cs_transport_order.
ENDINTERFACE.
