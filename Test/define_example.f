module define_example_m

  implicit none

  private compute_z_cv

contains

  subroutine define_example_reg(x_min, y_min, step_x, step_y, z, cv)

    use nr_util, only: arth
    
    real, intent(out):: x_min, y_min, step_x, step_y
    real, intent(out), allocatable:: z(:, :) ! (nx, ny)
    real, intent(out), allocatable, optional:: cv(:)

    ! Local:
    integer, parameter:: NX = 51, NY = 51
    real, parameter:: x_max = 2., Y_MAX = 2.

    !--------------------------------------------------

    X_MIN = - 2.
    Y_MIN = - 2.
    step_x = (X_MAX - X_MIN) / REAL(NX - 1)
    step_y = (Y_MAX - Y_MIN) / REAL(NY - 1)
    allocate(z(nx, ny))
    call compute_z_cv(arth(x_min, step_x, nx), arth(y_min, step_y, ny), z, cv)

  end subroutine define_example_reg

  !****************************************************************

  subroutine define_example_irreg(x, y, z, cv)

    use numer_rec_95, only: sort

    real, intent(out), allocatable:: x(:) ! (nx)
    real, intent(out), allocatable:: y(:) ! (ny)
    real, intent(out), allocatable:: z(:, :) ! (nx, ny)
    real, intent(out), allocatable, optional:: cv(:)

    ! Local:
    integer, parameter:: NX = 51, NY = 51
    real, parameter:: XMIN = - 2., XMAX = 2., YMIN = - 2., YMAX = 2.

    !--------------------------------------------------

    allocate(x(nx), y(ny), z(nx, ny))

    call random_number(x)
    call sort(x)
    x = xmin + x * (xmax - xmin)

    call random_number(y)
    call sort(y)
    y = ymin + y * (ymax - ymin)

    call compute_z_cv(x, y, z, cv)

  end subroutine define_example_irreg

  !****************************************************************

  subroutine compute_z_cv(x, y, z, cv)

    real, intent(in):: x(:), y(:)
    real, intent(out):: z(:, :) ! (size(x), size(y))
    real, intent(out), allocatable, optional:: cv(:)

    ! Local:
    integer i, j
    real, parameter:: MU = 0.3

    !--------------------------------------------------

    DO I=1, size(x)
       DO J=1, size(y)
          Z(I, J) = (1. - MU) * (2. / SQRT((X(I) - MU)**2 + Y(J)**2) + (X(I) &
               - MU)**2 + Y(J)**2) + MU * (2. / SQRT((X(I) + 1. - MU)**2 &
               + Y(J)**2) + (X(I) + 1. - MU)**2 + Y(J)**2)
       end DO
    end DO

    if (present(cv)) CV = [3., 3.05, 3.2, 3.5, 3.50135, 3.6, 3.766413, 4., &
         4.130149, 5., 10., maxval(z), 80.]

  end subroutine compute_z_cv

end module define_example_m
