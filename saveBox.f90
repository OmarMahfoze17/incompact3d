!############################################################################
!
subroutine saveBox (ux1,uy1,uz1,nx1,nx2,ny1,ny2,nz1,nz2)
!
!############################################################################
USE param
USE variables
USE decomp_2d
USE decomp_2d_io
implicit none

TYPE(DECOMP_INFO) :: phG
real(mytype),dimension(xsize(1),xsize(2),xsize(3)) :: ux1,uy1,uz1
integer :: code,icomplet,ip,jp,kp,ifile,nx1,nx2,ny1,ny2,nz1,nz2
integer :: ijk,nvect1,nvect2,nvect3,i,j,k,ss,kkkk
character(len=20) nfichier,nfichier1
character(len=20) :: filename

953 format('uxBox',I5.5)
write(*,*) "=========================================================================================================="
write (*,*) nrank,xstart(1),xend(1),xsize(1),xstart(2),xend(2),xsize(2),xstart(3),xend(3),xsize(3)
write(*,*) "=========================================================================================================="
ifile=10+nrank

do kp=nz1,nz2
    if (kp.ge.xstart(3).and.kp.le.xend(3)) then
    do jp=ny1,ny2
        if (jp .ge.xstart(2).and.jp.le.xend(2)) then

	ifile=10+nrank
	!close(ifile)
	if (itime==ifirst) then
	    write(filename, 953) (nrank+1)
	    open(ifile,file=filename,ACCESS='STREAM')
	endif 

		write(ifile) ux1(:,jp-xstart(2)+1,kp-xstart(3)+1)

	endif
    
    enddo
    endif
enddo
if (itime==ilast) then
 close(ifile)
endif

!############################################################################
end subroutine saveBox

