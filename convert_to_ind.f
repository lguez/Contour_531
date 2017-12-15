module convert_to_ind_m

  implicit none

  interface convert_to_ind

     ! For a regular grid. From position in coordinate space to
     ! position in index space. Position (1., 1.) in index space
     ! corresponds to corner.

     module procedure convert_to_ind_one, convert_to_ind_several
  end interface convert_to_ind

contains

  pure function convert_to_ind_one(coord, corner, step)

    real, intent(in):: coord(:), corner(:), step(:) ! (2)
    real convert_to_ind_one(2)

    !---------------------------------------------------

    convert_to_ind_one = (coord - corner) / step + 1.

  end function convert_to_ind_one

  !********************************************************************

  pure function convert_to_ind_several(coord, corner, step)

    real, intent(in):: coord(:, :) ! (2, :)
    real, intent(in):: corner(:), step(:) ! (2)
    real convert_to_ind_several(2, size(coord, 2))

    ! Local:
    integer j

    !---------------------------------------------------

    forall (j = 1:size(coord, 2)) &
         convert_to_ind_several(:, j) = (coord(:, j) - corner) / step + 1.

  end function convert_to_ind_several

end module convert_to_ind_m
