INTERFACE zif_ca_ttemplate_data
  PUBLIC .

  TYPES: tt_r_langu TYPE RANGE OF sylangu.

  CONSTANTS: BEGIN OF ts_section,
               subject TYPE zca_e_ttempl_section VALUE 'S',
               body    TYPE zca_e_ttempl_section VALUE 'B',
             END OF ts_section.

ENDINTERFACE.
