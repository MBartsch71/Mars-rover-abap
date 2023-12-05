##TODO
"1) Acceptance test #1 -> show the grid(10x10) where the rover will move
"2) Acceptance test #2 -> show the rover at the starting position
"3) Acceptance test #3 -> build a generic map in random size

REPORT ymbh_mars_rover.

CLASS world_map DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF grid_line,
             col1 TYPE char1,
             col2 TYPE char1,
             col3 TYPE char1,
           END OF grid_line.
    TYPES grid TYPE STANDARD TABLE OF grid_line WITH EMPTY KEY.

    METHODS get RETURNING VALUE(result) TYPE grid.

ENDCLASS.

CLASS world_map IMPLEMENTATION.

  METHOD get.
    result = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > 3
                        ( col1 = |.| col2 = |.| col3 = |.| ) ).
  ENDMETHOD.

ENDCLASS.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD run.
    DATA(grid) = NEW world_map( )->get( ).

    cl_demo_output=>display_data( grid ).
  ENDMETHOD.

ENDCLASS.

CLASS test_world_map DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF grid_line,
             col1 TYPE char1,
             col2 TYPE char1,
             col3 TYPE char1,
           END OF grid_line.
    TYPES grid TYPE STANDARD TABLE OF grid_line WITH EMPTY KEY.

    METHODS can_create_object FOR TESTING.
    METHODS build_a_3x3_grid FOR TESTING.

ENDCLASS.

CLASS test_world_map IMPLEMENTATION.

  METHOD can_create_object.
    DATA(cut) = NEW world_map( ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD build_a_3x3_grid.
    DATA(cut) = NEW world_map( ).
    DATA(expected_values) = VALUE grid( ( col1 = |.| col2 = |.| col3 = |.| )
                                        ( col1 = |.| col2 = |.| col3 = |.| )
                                        ( col1 = |.| col2 = |.| col3 = |.| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW main( )->run( ).
