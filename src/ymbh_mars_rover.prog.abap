##TODO
"1) Acceptance test #1 -> show the grid(10x10) where the rover will move
"2) Acceptance test #2 -> show the rover at the starting position
"3) Acceptance test #3 -> build a generic map in random size

REPORT ymbh_mars_rover.

CLASS grid_table DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF table_grid_line,
             row   TYPE i,
             col   TYPE i,
             value TYPE char1,
           END OF table_grid_line.
    TYPES table_grid TYPE SORTED TABLE OF table_grid_line WITH NON-UNIQUE KEY primary_key COMPONENTS row col.
    METHODS constructor IMPORTING rows TYPE i
                                  cols TYPE i.
    METHODS get_lines RETURNING VALUE(result) TYPE i.
    METHODS build_display_table
      RETURNING
        VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    DATA table TYPE table_grid.

ENDCLASS.

CLASS grid_table IMPLEMENTATION.

  METHOD constructor.
    table = VALUE #( FOR i = 1 THEN i + 1 WHILE i <= 10
                     FOR j = 1 THEN j + 1 WHILE j <= 10
                       ( row = i col = j value = |.| ) ).
  ENDMETHOD.


  METHOD get_lines.
    result = lines( table ).
  ENDMETHOD.


  METHOD build_display_table.
    DATA display_line TYPE string.
    LOOP AT table REFERENCE INTO DATA(line) GROUP BY line->row.
      LOOP AT GROUP line REFERENCE INTO DATA(cols).
        display_line = |{ display_line }{ cols->value }|.
      ENDLOOP.
      result = VALUE #( BASE result ( display_line ) ).
      CLEAR display_line.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS world_map DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS get_grid_display
      RETURNING
        VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    DATA grid TYPE REF TO grid_table.
ENDCLASS.

CLASS world_map IMPLEMENTATION.

  METHOD constructor.
    grid = NEW #( rows = 10 cols = 10 ).
  ENDMETHOD.

  METHOD get_grid_display.
    result = grid->build_display_table( ).
  ENDMETHOD.

ENDCLASS.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD run.
    DATA(grid) = NEW world_map( )->get_grid_display( ).
    cl_demo_output=>display_data( grid ).
  ENDMETHOD.

ENDCLASS.


CLASS test_world_map DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES: BEGIN OF grid_line,
             col1  TYPE char1,
             col2  TYPE char1,
             col3  TYPE char1,
             col4  TYPE char1,
             col5  TYPE char1,
             col6  TYPE char1,
             col7  TYPE char1,
             col8  TYPE char1,
             col9  TYPE char1,
             col10 TYPE char1,
           END OF grid_line.
    TYPES grid TYPE STANDARD TABLE OF grid_line WITH EMPTY KEY.

    METHODS can_create_object FOR TESTING.
    METHODS display_10x10_grid FOR TESTING.




    METHODS get_grid
      RETURNING
        VALUE(result) TYPE grid.

ENDCLASS.

CLASS test_world_map IMPLEMENTATION.

  METHOD can_create_object.
    DATA(cut) = NEW world_map( ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD get_grid.
    result = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > 10
                        (  col1 = |.| col2 = |.| col3 = |.| col4 = |.| col5 = |.| col6 = |.| col7 = |.| col8 = |.| col9 = |.| col10 = |.| ) ).

  ENDMETHOD.

  METHOD display_10x10_grid.
    DATA(cut) = NEW world_map( ).
    DATA(expected_values) = VALUE stringtab( ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| )
                                             ( |..........| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_grid_display( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS test_grid_table DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS build_10x10_grid_table FOR TESTING.
ENDCLASS.

CLASS test_grid_table IMPLEMENTATION.

  METHOD build_10x10_grid_table.
    DATA(grid_table) = NEW grid_table( rows = 10 cols = 10 ).
    cl_abap_unit_assert=>assert_equals( exp = 100 act = grid_table->get_lines( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW main( )->run( ).
