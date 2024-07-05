module convert_to_ind_m

  implicit none

contains

  pure function convert_to_ind(coord, corner, step)

    ! For a regular grid. From position in coordinate space to
    ! position in index space, base 1. That is, position (1., 1.) in
    ! index space corresponds to corner.

    real, intent(in):: coord(:, :) ! (2, :)
    real, intent(in):: corner(:), step(:) ! (2)
    real convert_to_ind(2, size(coord, 2))

    ! Local:
    integer j

    !---------------------------------------------------

    forall (j = 1:size(coord, 2)) &
         convert_to_ind(:, j) = (coord(:, j) - corner) / step + 1.

  end function convert_to_ind

end module convert_to_ind_m
