PROGRAM integral
    IMPLICIT NONE

    DOUBLE PRECISION :: area, col_width, col_height, next_col_height 
    DOUBLE PRECISION :: shape_area, n_cols, b, a, actual_column, next_column
    INTEGER :: i = 0, method

    area = 0
    a = 3
    b = 8
    n_cols = 5

    PRINT *, "Select the method:"
    PRINT *, "1. Rectangles"
    PRINT *, "2. Rectangles with triangles on top"
    PRINT *, "3. Rectangles with semicircles on top"
    PRINT *, "4. Trapezoids"
    READ *, method

    col_width = (b - a) / n_cols

    DO
        IF (i >= n_cols) EXIT

        actual_column = DBLE(i)
        next_column = DBLE(i + 1)
        
        col_height = calculate_function(a + actual_column * col_width)
        next_col_height = calculate_function(a + next_column * col_width)
        
        SELECT CASE (method)
            CASE (1)
                shape_area = calculate_rectangle_area(col_width, col_height)
            CASE (2)
                shape_area = calculate_triangle_rectangle_area(col_width, col_height, next_col_height)
            CASE (3)
                shape_area = calculate_semicircle_rectangle_area(col_width, col_height, next_col_height)
            CASE DEFAULT
                shape_area = calculate_trapezoid_area(col_width, col_height, next_col_height)
        END SELECT

        PRINT *, "Shape", i + 1, "area =", shape_area
        area = area + shape_area
        i = i+1
    END DO

    PRINT *, "Area = ", area

    CONTAINS
        DOUBLE PRECISION FUNCTION calculate_function(x)
            IMPLICIT NONE
            DOUBLE PRECISION :: x
            
            calculate_function = x ** 3 + 2 * x ** 2 + 3 * x - 4
            
            RETURN
        END FUNCTION calculate_function

        DOUBLE PRECISION FUNCTION calculate_rectangle_area(base, height)
            IMPLICIT NONE
            DOUBLE PRECISION :: base, height
            
            calculate_rectangle_area = base * height
            RETURN
        END FUNCTION calculate_rectangle_area

        DOUBLE PRECISION FUNCTION calculate_triangle_rectangle_area(base, height, next_col_height)
            IMPLICIT NONE
            DOUBLE PRECISION :: base, height, next_col_height
            
            calculate_triangle_rectangle_area = base * height + 0.5 * base * (next_col_height - height)
            RETURN
        END FUNCTION calculate_triangle_rectangle_area

        DOUBLE PRECISION FUNCTION calculate_semicircle_rectangle_area(base, height, next_col_height)
            IMPLICIT NONE
            DOUBLE PRECISION :: base, height, next_col_height, radius
            
            radius = abs(height - next_col_height) / 2
            calculate_semicircle_rectangle_area = base * min(height, next_col_height) + 3.14159265358979323846 * radius**2 / 2
            RETURN
        END FUNCTION calculate_semicircle_rectangle_area

        DOUBLE PRECISION FUNCTION calculate_trapezoid_area(base, height, next_col_height)
            IMPLICIT NONE
            DOUBLE PRECISION :: base, height, next_col_height
            
            calculate_trapezoid_area = ((height + next_col_height) * base) / 2
            RETURN
        END FUNCTION calculate_trapezoid_area

END PROGRAM integral
