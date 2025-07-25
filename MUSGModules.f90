MODULE GLOBAL
    use KindParameters
    implicit none
    integer(i4), PARAMETER :: NIUNIT=100
    integer(i4),save::iunsat
    integer(i4),SAVE ::NCOL,NROW,NLAY,NPER,NBOTM,NCNFBD,IVSD
    integer(i4), SAVE ::ITMUNI,LENUNI,IXSEC,ITRSS,INBAS,IUNSTR
    integer(i4), SAVE :: IDEALLOC_LPF,IDEALLOC_HY,ISYMFLG
    integer(i4), SAVE :: ITRNSP,NEQS,NODES,NJA,NJAS,NJAG
    integer(i4), SAVE :: IOUT,IPRCONN,IDSYMRD,ILAYCON4,IWADI,IWADICLN,IDPF,IDPT,INSGB, ImdT,IDPIN,IDPOUT,IHMSIM,IUIHM,ISYALL
    integer(i4), SAVE :: INCLN,INGNC,INGNC2,INGNCn, INDDF
    integer(i4), SAVE :: INSWF
    integer(i4), SAVE :: IFMBC,MBEGWUNF,MBEGWUNT,MBECLNUNF,MBECLNUNT,IOUTNORMAL 
    integer(i4), SAVE ::IFREFM,MXNODLAY,ICONCV,NOVFC
    integer(i4), SAVE, ALLOCATABLE :: NOCVCO
    real(dp), SAVE::WADIEPS
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IUNIT(:)
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IBOUND
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::gIA
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IAG
    real(dp), DIMENSION(:),ALLOCATABLE  :: FLOWJA(:),FMBE(:)
    integer(i4), DIMENSION(:),ALLOCATABLE :: IATMP
    integer(i4) :: NJATMP
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::JA
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE ::JAFL          
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::JAS
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::ISYM
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IVC
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::HOLD
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::HNEW
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::RHS
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE ::AMAT
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::Sn,So
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::AKRC,AKR
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::TURBGRAD
    real(sp), DIMENSION(:),ALLOCATABLE  ::DELR
    real(sp), DIMENSION(:),ALLOCATABLE  ::DELC
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::BOT
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::TOP
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::PGF
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::AREA
    real(sp), SAVE, DIMENSION(:),ALLOCATABLE  ::FAHL        
    real(sp), SAVE, DIMENSION(:),ALLOCATABLE  ::CL1,CL2
    real(sp), SAVE, DIMENSION(:),ALLOCATABLE  ::ARAD
    integer(i4), SAVE,    DIMENSION(:),    ALLOCATABLE  ::NODLAY
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:)     ::LAYNOD
    integer(i4), DIMENSION(:),    ALLOCATABLE  ::LAYCBD
    integer(i4), SAVE,    DIMENSION(:),    ALLOCATABLE  ::LAYHDT
    integer(i4), SAVE,    DIMENSION(:),    ALLOCATABLE  ::LAYHDS
    real(dp),  SAVE, DIMENSION(:),  ALLOCATABLE  ::PERLEN
    integer(i4), SAVE,    DIMENSION(:),    ALLOCATABLE  ::NSTP
    real(sp),    SAVE,    DIMENSION(:),    ALLOCATABLE  ::TSMULT
    integer(i4), SAVE,    DIMENSION(:),    ALLOCATABLE  ::ISSFLG
    real(sp),    SAVE,    DIMENSION (:),   ALLOCATABLE  :: STORFRAC
    real(sp),    SAVE,    DIMENSION(:),ALLOCATABLE  ::BUFF
    real(sp),    SAVE,    DIMENSION(:),ALLOCATABLE  ::STRT
    real(dp),  SAVE,  ALLOCATABLE,   DIMENSION(:) ::HWADI
    real(dp),  SAVE,  ALLOCATABLE,   DIMENSION(:) ::DWADI
    real(sp),    SAVE,    DIMENSION(:),ALLOCATABLE ::DDREF
    integer(i4),           SAVE, DIMENSION(:),  ALLOCATABLE ::NODETIBH
    integer(i4) :: NIB1
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE  ::HTIB
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IA2
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::JA2
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IA1IA2
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::JA1JA2
    integer(i4), SAVE :: NEQS2,NJA2
END MODULE GLOBAL

! -----------------------------------------------------------------------
MODULE NAMEFILEMODULE
    use KindParameters
    implicit none
    CHARACTER*300 SYFNAME, fnames(5000)
    integer(i4) SYIU,SYIFLEN,ius(5000),iflens(5000),nfiles,IARCVs(5000)
    CHARACTER*4 CUNITs(5000)
    CHARACTER*7 FILSTATs(5000)
    CHARACTER*20 FILACTs(5000), FMTARGs(5000), ACCARGs(5000)
END MODULE NAMEFILEMODULE

!------------------------------------------------------------------
MODULE PARAMMODULE
    use KindParameters
    implicit none
    !  Data definitions for Named Parameters
    !  Explicitly declare all variables to enable subroutines that include
    !  this file to use the IMPLICIT NONE statement.
    integer(i4), PARAMETER :: MXPAR=999,MXCLST=1000,MXINST=1000
    integer(i4),SAVE ::ICLSUM,IPSUM,INAMLOC,NMLTAR,NZONAR,NPVAL
    real(sp),          SAVE,    DIMENSION(:),    ALLOCATABLE ::B
    integer(i4),       SAVE,    DIMENSION(:),    ALLOCATABLE ::IACTIVE
    integer(i4),       SAVE,    DIMENSION(:,:),  ALLOCATABLE ::IPLOC
    integer(i4),       SAVE,    DIMENSION(:,:),  ALLOCATABLE ::IPCLST
    integer(i4),       SAVE,    DIMENSION(:,:,:),ALLOCATABLE ::IZON
    real(sp),          SAVE,    DIMENSION(:,:,:),ALLOCATABLE ::RMLT
    CHARACTER(LEN=10),SAVE, DIMENSION(:),    ALLOCATABLE ::PARNAM
    CHARACTER(LEN=4), SAVE, DIMENSION(:),    ALLOCATABLE ::PARTYP
    CHARACTER(LEN=10),SAVE, DIMENSION(:),    ALLOCATABLE ::ZONNAM
    CHARACTER(LEN=10),SAVE, DIMENSION(:),    ALLOCATABLE ::MLTNAM
    CHARACTER(LEN=10),SAVE, DIMENSION(:),    ALLOCATABLE ::INAME
END MODULE PARAMMODULE

!------------------------------------------------------------------
MODULE GWFBASMODULE
    use KindParameters
    implicit none
    integer(i4), SAVE  ::MSUM
    integer(i4), SAVE  ::IHEDFM,IHEDUN,IDDNFM,IDDNUN,IBOUUN,ISPCFM,ISPCUN
    integer(i4), SAVE  ::LBHDSV,LBDDSV,LBBOSV
    integer(i4), SAVE  ::IBUDFL,ICBCFL,IHDDFL,ISPCFL,IAUXSV,IBDOPT,IATS,IBUDFLAT,ICBCFLAT,IHDDFLAT,ISPCFLAT
    integer(i4), SAVE :: IFAST,ISPFAST,ITSFAST,IUGFAST,IUCFAST,IUDFAST,IFASTH,IFASTC,ISPFASTC,ITSFASTC,IUGFASTC,IUCFASTC, &
        IUDFASTC,IUMFASTC  
    integer(i4), SAVE  ::IPRTIM,IPEROC,ITSOC,ICHFLG,IFRCNVG
    real(dp),  SAVE  ::DELT,PERTIM,TOTIM
    real(dp), SAVE :: DELTAT,TMINAT,TMAXAT,TADJAT,TCUTAT
    integer(i4), SAVE ::NPTIMES,NPSTPS,ITIMOT,ITIMOTC,IUGBOOT,IUCBOOT,IUDBOOT,IBOOT,IBOOTSCALE
    real(dp), SAVE :: DTBOOTSCALE
    real(dp), DIMENSION(:),ALLOCATABLE  ::BOOTSLOPE
    real(dp), DIMENSION(:),ALLOCATABLE  ::BOOTSCALE
    real(dp), DIMENSION(:),ALLOCATABLE  ::HREADBOOT       
    real(dp),  SAVE, DIMENSION(:),ALLOCATABLE ::TIMOT,TIMOTC
    real(sp),              SAVE  ::HNOFLO
    CHARACTER(LEN=20), SAVE   ::CHEDFM,CDDNFM,CBOUFM,CSPCFM
    integer(i4),           SAVE, DIMENSION(:,:), ALLOCATABLE ::IOFLG
    real(dp),  SAVE, DIMENSION(:,:), ALLOCATABLE ::VBVL
    CHARACTER(LEN=16), SAVE, DIMENSION(:),   ALLOCATABLE ::VBNM
    integer(i4), SAVE  ::IDDREF,IDDREFNEW
    integer(i4),           SAVE, DIMENSION(:),  ALLOCATABLE  ::IFLUSHS !kkz for list of binary output unit numbers to flush
    integer(i4),           SAVE   ::CFLUSH !kkz - count of binary file unit numbers to flush at ends of time steps        
END MODULE GWFBASMODULE

!------------------------------------------------------------------
MODULE GWFBCFMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE ::IBCFCB,IWDFLG,IWETIT,IHDWET,IKCFLAG,IKVFLAG,IBPN,IDRY,ITABRICH,INTRICH,NUTABROWS,NUZONES 
    real(sp), SAVE    ::WETFCT,HDRY
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:)     ::LAYCON
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:)     ::LAYAVG,LAYAVGV
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::CV
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::SC1
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::SC2
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::WETDRY
    real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) ::alpha,beta,sr,brook,BP
    integer(i4), SAVE, ALLOCATABLE, DIMENSION(:) :: IUZONTAB
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: RETCRVS
    integer(i4), SAVE,   ALLOCATABLE, DIMENSION(:)     ::LAYWET
    integer(i4), SAVE ::ISFAC,ITHFLG,IHANISO,IALTSTO
    integer(i4), SAVE,   ALLOCATABLE, DIMENSION(:)     ::LAYTYP
    real(sp),    SAVE,   ALLOCATABLE, DIMENSION(:)     ::CHANI
    integer(i4), SAVE,   ALLOCATABLE, DIMENSION(:)     ::LAYVKA
    integer(i4), SAVE,   ALLOCATABLE, DIMENSION(:)     ::LAYSTRT
    integer(i4), SAVE,   ALLOCATABLE, DIMENSION(:,:)   ::LAYFLG
    real(sp),    SAVE,   ALLOCATABLE, DIMENSION(:) ::VKA
    real(sp),    SAVE,   ALLOCATABLE, DIMENSION(:) ::VKCB
    real(sp),    SAVE,   ALLOCATABLE, DIMENSION(:) ::HANI
    real(sp),    SAVE,   ALLOCATABLE, DIMENSION(:) ::HK
    real(dp),  SAVE,  ALLOCATABLE,  DIMENSION(:) :: HWADIGW 
    real(dp),  SAVE,  ALLOCATABLE,  DIMENSION(:) :: DWADIGW
END MODULE GWFBCFMODULE

!------------------------------------------------------------------
MODULE CLN1MODULE
    use KindParameters
    implicit none
    integer(i4), SAVE :: NCLN,ICLNCB,ICLNHD,ICLNDD,ICLNIB, &
        NJA_CLN, NCLNNDS,NCLNGWC,NCONDUITYP,NRECTYP,ICLNTIB, &   !aq CLN CCF
        ICLNPCB,ICLNGWCB,ICLNCN,ICLNMB,IBHETYP
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::NNDCLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IFLINCLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::ICCWADICLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::ICGWADICLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::CLNCON
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IA_CLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::JA_CLN
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IDXGLO_CLN
    real(dp), SAVE,    DIMENSION(:,:),ALLOCATABLE  ::ACLNNDS
    real(sp), SAVE,    DIMENSION(:,:),ALLOCATABLE  ::ACLNGWC
    real(dp), SAVE,   DIMENSION(:,:),ALLOCATABLE  ::ACLNCOND
    real(sp), SAVE,    DIMENSION(:,:),ALLOCATABLE  :: BHEPROP
        real(sp), SAVE,    DIMENSION(:,:),ALLOCATABLE  ::ACLNREC
    !-------FOR FLOW TO DRY CELL FORMULATION        
    real(dp),  SAVE, ALLOCATABLE,  DIMENSION(:) ::HWADICC
    real(dp),  SAVE, ALLOCATABLE,  DIMENSION(:) ::HWADICG
    real(dp),  SAVE, ALLOCATABLE,  DIMENSION(:) ::DWADICC
    real(dp),  SAVE, ALLOCATABLE,  DIMENSION(:) ::DWADICG
    !-------FOR TURBULENT FLOW FORMULATION 
    real(sp), SAVE :: GRAV, VISK
END MODULE CLN1MODULE

!------------------------------------------------------------------
MODULE SWF1MODULE
    use KindParameters
    implicit none
    integer(i4), SAVE, POINTER :: ISWFCB,ISWFHD,ISWFDD,ISWFIB,NJA_SWF,NSWFNDS,NSWFGWC,NSWFTYP,ISWFTIB, &
            ISWFPCB,ISWFGWCB,ISWFCN,ISWFMB
    real(dp) :: MIN_DEPTH = 1.0D-8
    integer(i4), SAVE,    DIMENSION(:),POINTER  ::IA_SWF
    integer(i4), SAVE,    DIMENSION(:),POINTER  ::JA_SWF
    integer(i4), SAVE,    DIMENSION(:),POINTER  ::IDXGLO_SWF
    real(dp), SAVE,    DIMENSION(:,:),ALLOCATABLE  ::ASWFNDS
    real(sp), SAVE,    DIMENSION(:,:),ALLOCATABLE  ::ASWFGWC
    real(dp), SAVE,   DIMENSION(:,:),ALLOCATABLE  ::ASWFCOND
    !real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) :: BOTSWF
    !real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) :: AREASWF
    !real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) :: CLSGSWF
    !real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) :: MANNSWF
    !real(sp), SAVE,  ALLOCATABLE,   DIMENSION(:) :: MICHSWF
    real(sp), SAVE, DIMENSION(:),ALLOCATABLE  ::FAHL_SWF        
    real(sp), SAVE, DIMENSION(:),ALLOCATABLE  ::CL1_SWF,CL2_SWF,CL12_SWF
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::ISSWADISWF
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::ISGWADISWF
    integer(i4), SAVE,    DIMENSION(:),ALLOCATABLE  ::IFLINSWF
END MODULE SWF1MODULE

!------------------------------------------------------------------
MODULE SMSMODULE
    use KindParameters
    implicit none
    real(dp), PARAMETER :: EPSILON = 1.0E-6
    real(dp), PARAMETER :: CLOSEZERO = 1.0E-15      
    real(dp), SAVE :: Theta
    real(dp), SAVE :: Akappa, Gamma, Amomentum,Breduc, Btol,RES_LIM
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::AMATFL
    real(dp), SAVE :: HCLOSE, HICLOSE,BIGCHOLD,BIGCH
    integer(i4), SAVE :: ITER1,MXITER,LINMETH,NONMETH,Numtrack,IPRSMS,IBFLAG     
    real(dp), DIMENSION(:),ALLOCATABLE  :: CELLBOTMIN
    real(dp), DIMENSION(:),ALLOCATABLE  ::HTEMP
    !SP - THIS IS AMAT      real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: A
    !SP - THIS IS RHS      real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: B
    !SP - THIS IS HNEW      real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: Hchange
    integer(i4), SAVE, DIMENSION(:,:), ALLOCATABLE ::  Lrch
    integer(i4), SAVE, DIMENSION(:), ALLOCATABLE ::  LrchL
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: Hncg
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE ::  HNCGL
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: DKDH,DKDHC
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: DTURBGRAD
    real(dp),SAVE,DIMENSION(:),ALLOCATABLE::DEold,Wsave,Hchold
    integer(i4) ISOLVEACTIVE, IBOTAV,ISHIFT
    real(dp) scalfact
    ! DM: Forcing term variables
    real(dp), SAVE ::Rcutoff,ForcingAlpha,ForcingGamma
    real(dp), SAVE::MaxRcutoff
    integer(i4), SAVE      ::ICUTOFF,NoMoreRcutoff,ITRUNCNEWTON
    ! End DM
END MODULE SMSMODULE

!------------------------------------------------------------------
MODULE TVMU2MODULE
    use KindParameters
    implicit none
    integer(i4),SAVE :: ITVMPRINT,NTVMHK,NTVMVKA,NTVMSS,NTVMSY,NTVMPOR
    integer(i4),SAVE :: NTVMSCHANGES
    integer(i4),SAVE :: NTVMDDFTR
    integer(i4),SAVE :: IHAVESC2
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMHK,ITVMVKA
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMSS,ITVMSY
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMPOR
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMDDFTR
    real(sp),SAVE :: TVMLOGBASEHK,TVMLOGBASEHKINV
    real(sp),SAVE :: TVMLOGBASEVKA,TVMLOGBASEVKAINV
    real(sp),SAVE :: TVMLOGBASESS,TVMLOGBASESSINV
    real(sp),SAVE :: TVMLOGBASESY,TVMLOGBASESYINV
    real(sp),SAVE :: TVMLOGBASEPOR,TVMLOGBASEPORINV
    real(sp),SAVE :: TVMDDFTR,TVMDDFTRINV
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: HKNEWA,VKANEWA
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: HKNEWB,VKANEWB
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: SSNEWA,SYNEWA
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: SSNEWB,SYNEWB
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: SSOLD,SYOLD
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: PORNEWA
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: PORNEWB
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: POROLD
    real(sp),SAVE,ALLOCATABLE,DIMENSION(:) :: DDFTRNEWA,DDFTRNEWB
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMSMAPSS
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMSMAPSY
    integer(i4),SAVE,ALLOCATABLE,DIMENSION(:) :: ITVMSMAPPOR
    real(dp),SAVE,ALLOCATABLE,DIMENSION(:) :: INITPGF
END MODULE

!------------------------------------------------------------------
MODULE GWTBCTMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE ::IBCTCB,MCOMP,ITVD,IADSORB,ICT,NTITER,IDISP, &
        IXDISP,IZOD,IFOD,IHEAT,MCOMPT,NTCOMP,IDISPCLN,IMULTI,NSEQITR, &
        ISOLUBILITY,IAW_ADSORB
    real(sp), SAVE    ::CINACT
    real(dp), SAVE ::CICLOSE
    real(sp), SAVE ::DIFFNC
    integer(i4), SAVE, DIMENSION(:,:), ALLOCATABLE ::  LrcC
    integer(i4),    SAVE, DIMENSION(:), ALLOCATABLE     ::IPCBFLAG
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: Cncg
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:)     ::ICBUND
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::PRSITY
    real(dp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::MASSBCT
    real(sp), SAVE, ALLOCATABLE,   DIMENSION(:,:) ::ADSORB,FLICH,ZODRW,FODRW,ZODRS,FODRS
    real(sp), SAVE, ALLOCATABLE,   DIMENSION(:,:) ::VELNOD
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) :: VXL, VYL       
    real(sp), SAVE, ALLOCATABLE,  DIMENSION(:) :: DLX,DLY,DLZ,ATXY,ATYZ,ATXZ
    real(sp), SAVE, ALLOCATABLE,  DIMENSION(:) :: ALXY,ALYZ,ALXZ
    real(dp), SAVE,  ALLOCATABLE,DIMENSION(:,:) ::CONC,CONCO
    real(dp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::CBCF
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::CBCH
    real(dp),    SAVE, DIMENSION(:,:,:), ALLOCATABLE ::VBVLT
    CHARACTER(LEN=16), SAVE, DIMENSION(:,:),   ALLOCATABLE ::VBNMT
    integer(i4), SAVE  ::MSUMT
    integer(i4), SAVE, DIMENSION(:), ALLOCATABLE ::  FLrcC
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: FCncg
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE :: ADMAT
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::DXCS,DYCS,DZCS
    real(sp), SAVE,     ALLOCATABLE,   DIMENSION(:) ::  DCXL,DCYL
    integer(i4),           SAVE, DIMENSION(:),  ALLOCATABLE ::NODETIBC
    integer(i4) :: NICB1
    real(dp), SAVE, DIMENSION(:,:),ALLOCATABLE  ::CTIB
    real(sp), SAVE :: HTCONDW, RHOW, HTCAPW 
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:) :: HTCONDM, HTCONDS,HTCONDT
    CHARACTER (LEN=200), SAVE :: CROOTNAME
    real(dp), SAVE :: TIMEWEIGHT
    integer(i4), SAVE :: ICHAIN,ISPRCT,ISATADSORB
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:) :: NPARENT
    integer(i4), SAVE,  ALLOCATABLE,   DIMENSION(:,:) :: JPARENT
    real(sp), SAVE, ALLOCATABLE, DIMENSION (:,:) :: STOTIO
    real(sp), SAVE, ALLOCATABLE, DIMENSION (:,:,:) :: SPTLRCT
    real(sp), SAVE, ALLOCATABLE,   DIMENSION(:) :: SOLLIM, SOLSLOPE
END MODULE GWTBCTMODULE

!------------------------------------------------------------------
MODULE GWTDPTMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE ::IDPTCB,IDPTCON,IC_IBNDIN_FLG,IADSORBIM,IDISPIM,IZODIM,IFODIM,IAW_ADSORBIM
    integer(i4), SAVE, ALLOCATABLE, DIMENSION (:) :: ICBUNDIM
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:) ::DDTTR
    !
    real(dp),SAVE,ALLOCATABLE,DIMENSION(:,:) :: CONCIM
    real(dp),SAVE,ALLOCATABLE,DIMENSION(:,:) :: CONCOIM
    real(sp), SAVE,ALLOCATABLE,DIMENSION(:) :: PRSITYIM,BULKDIM,DLIM
    real(sp), SAVE, ALLOCATABLE,   DIMENSION(:,:) ::ADSORBIM,FLICHIM,ZODRWIM,FODRWIM,ZODRSIM,FODRSIM
    real(sp), SAVE, ALLOCATABLE, DIMENSION (:,:,:) :: SPTLRCTIM
    real(dp), SAVE,ALLOCATABLE,DIMENSION(:) :: MASSBCTIM
    real(dp), SAVE,ALLOCATABLE,DIMENSION(:) :: DIADDT,RDDT
    real(dp), SAVE,ALLOCATABLE,DIMENSION(:) :: OFFDDTM
    real(dp), SAVE,ALLOCATABLE,DIMENSION(:) :: OFFDDTIM
END MODULE GWTDPTMODULE

!------------------------------------------------------------------
MODULE AW_ADSORBMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE :: IAREA_FN,IKAWI_FN,ITAB_AWI,NAZONES,NATABROWS
    integer(i4), SAVE, ALLOCATABLE, DIMENSION(:) :: IAWIZONMAP
    real(sp), SAVE :: ROG_SIGMA,SIGMA_RT 
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:) :: AWAMAX,AWAREA_X2,AWAREA_X1,AWAREA_X0,AREA_AWI,AREA_AWIO
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:) :: ALANGAW,BLANGAW,AK_AWI
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: AWI_AREA_TAB
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:,:,:) :: AWI_KAWI_TAB
END MODULE

!------------------------------------------------------------------
MODULE AW_ADSORBIMMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE :: IAREA_FNIM,IKAWI_FNIM,ITAB_AWIIM,NAZONESIM, NATABROWSIM
    integer(i4), SAVE, ALLOCATABLE, DIMENSION(:) :: IAWIZONMAPIM 
    real(sp), SAVE :: SIGMA_RTIM,ROG_SIGMAIM
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:) :: AWAMAXIM,AWAREA_X2IM,AWAREA_X1IM,AWAREA_X0IM,AREA_AWIIM,AREA_AWIIMO
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:) :: ALANGAWIM,BLANGAWIM,AK_AWIIM       
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: AWI_AREA_TABIM
    real(sp), SAVE, ALLOCATABLE, DIMENSION(:,:,:,:) :: AWI_KAWI_TABIM
END MODULE

!------------------------------------------------------------------
MODULE GWFWELMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE,POINTER ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL,IAFR
    integer(i4),SAVE,POINTER  ::NPWEL,IWELPB,NNPWEL,IWELQV,NNPWCLN,IWELLBOT,NAUXWEL
    CHARACTER(LEN=16),SAVE, DIMENSION(:),   ALLOCATABLE     ::WELAUX
    real(sp),             SAVE, DIMENSION(:,:), ALLOCATABLE     ::WELL
    real(dp), SAVE, DIMENSION(:,:), ALLOCATABLE :: WELLBOT
END MODULE GWFWELMODULE

!------------------------------------------------------------------
MODULE GWFCHDMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE,POINTER  ::NCHDS,MXCHD,NCHDVL,IPRCHD
    integer(i4),SAVE,POINTER  ::NPCHD,ICHDPB,NNPCHD
    CHARACTER(LEN=16),SAVE, DIMENSION(:),   ALLOCATABLE     ::CHDAUX
    real(sp),             SAVE, DIMENSION(:,:), ALLOCATABLE     ::CHDS
END MODULE

!------------------------------------------------------------------
MODULE GWFRCHMODULE
    use KindParameters
    implicit none
    integer(i4), SAVE, POINTER                 ::NRCHOP,IRCHCB,MXNDRCH
    integer(i4),SAVE, POINTER ::NPRCH,IRCHPF,INIRCH,NIRCH,mxznrch,ISELEV
    integer(i4), SAVE,POINTER::IPONDOPT,IRTSOPT,IRTSRD,INRTS,ICONCRCHOPT
    real(sp),    SAVE,   DIMENSION(:),  ALLOCATABLE ::RECH,SELEV,RECHSV
    integer(i4), SAVE,   DIMENSION(:),  ALLOCATABLE ::IRCH,iznrch
    real(sp),    SAVE,   DIMENSION(:),  ALLOCATABLE ::rtsrch
    real(sp),    SAVE,   DIMENSION(:),  ALLOCATABLE      ::RCHF
    real(sp),    SAVE,   DIMENSION(:,:),  ALLOCATABLE      ::RCHCONC  !DIMENSION NEQS,NCONCRCH
    integer(i4), SAVE,   DIMENSION(:),  ALLOCATABLE ::IRCHCONC     !DIMENSION MCOMP
    real(dp), SAVE, POINTER :: tstartrch,tendrch,factrrch,TIMRCH
END MODULE GWFRCHMODULE

!------------------------------------------------------------------
MODULE GWFDRNMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE,POINTER  ::NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN
    integer(i4),SAVE,POINTER  ::NPDRN,IDRNPB,NNPDRN
    CHARACTER(LEN=16),SAVE, DIMENSION(:),   ALLOCATABLE     ::DRNAUX
    real(sp),             SAVE, DIMENSION(:,:), ALLOCATABLE     ::DRAI

    TYPE GWFDRNTYPE
        integer(i4),POINTER  ::NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN
        integer(i4),POINTER  ::NPDRN,IDRNPB,NNPDRN
        CHARACTER(LEN=16), DIMENSION(:),   ALLOCATABLE     ::DRNAUX
        real(sp),              DIMENSION(:,:), ALLOCATABLE     ::DRAI
    END TYPE

    TYPE(GWFDRNTYPE), SAVE:: GWFDRNDAT(10)
END MODULE GWFDRNMODULE

!------------------------------------------------------------------
MODULE GNCnMODULE
    use KindParameters
    implicit none
    integer(i4), POINTER::MXGNCn,NGNCn,IPRGNCn,NGNCNPn,NPGNCn,IGNCPBn,I2Kn,ISYMGNCn,MXADJn,IFLALPHAn
    real(sp),    DIMENSION(:,:), ALLOCATABLE   ::GNCn,SATCn
    integer(i4), DIMENSION (:), ALLOCATABLE :: LGNCn
    integer(i4), DIMENSION (:,:,:),  ALLOCATABLE :: IRGNCn
END MODULE GNCnMODULE

!------------------------------------------------------------------
MODULE XMDMODULE
    use KindParameters
    implicit none
    LOGICAL, SAVE, POINTER ::  REDSYS
    LOGICAL, SAVE, POINTER :: ILUREUSE
    real(dp), SAVE, POINTER ::  EPSRN,RRCTOL
    real(dp), SAVE, DIMENSION(:),ALLOCATABLE::DGSCAL
    integer(i4), SAVE, DIMENSION(:), ALLOCATABLE ::  LORDER
    integer(i4), SAVE, POINTER :: IACL,NORDER,LEVEL,NORTH,IDROPTOL,IERR,IDSCALE
END MODULE XMDMODULE

!------------------------------------------------------------------
MODULE PCGUMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE,POINTER  :: ILINMETH
    PRIVATE
    !PUBLIC :: PCGU7U1AR
    !PUBLIC :: PCGU7U1AP
    !PUBLIC :: PCGU7U1DA
    !PUBLIC :: PCGUSETDIMS
    PUBLIC :: ILINMETH
    integer(i4),SAVE,POINTER  :: ITER1C,IPC,ISCL,IORD,NITERC,NNZC,NIAC
    integer(i4),SAVE,POINTER  :: NIABCGS
    integer(i4),SAVE,POINTER  :: NIAPC,NJAPC,NNZAPC
    real(sp),SAVE,POINTER  :: HCLOSEPCGU,RCLOSEPCGU
    real(sp),SAVE,POINTER  :: RELAXPCGU
    real(dp), SAVE, POINTER, DIMENSION(:)     :: DSCALE
    real(dp), SAVE, POINTER, DIMENSION(:)     :: DSCALE2
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: IAPC
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: JAPC
    real(dp), SAVE, POINTER, DIMENSION(:)     :: APC
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: LORDER
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: IORDER
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: IARO
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: JARO
    real(dp), SAVE, POINTER, DIMENSION(:)     :: ARO
    !         WORKING ARRAYS        
    integer(i4),         SAVE, POINTER, DIMENSION(:)      :: IWC
    real(dp), SAVE, POINTER, DIMENSION(:)     :: WC
    integer(i4),         SAVE, POINTER, DIMENSION(:)      :: ID
    real(dp), SAVE, POINTER, DIMENSION(:)     :: XC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: DC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: PC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: QC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: ZC
    !         BICGSTAB WORKING ARRAYS
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: TC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: VC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: DHATC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: PHATC
    DOUBLEPRECISION, SAVE, POINTER, DIMENSION(:)      :: QHATC
    !         POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: IA0
    integer(i4),          SAVE, POINTER, DIMENSION(:)     :: JA0
    real(dp), SAVE, POINTER, DIMENSION(:)     :: A0
        
    TYPE PCGUTYPE
        integer(i4),POINTER  :: ILINMETH
        integer(i4),POINTER  :: ITER1C,IPC,ISCL,IORD,NITERC,NNZC,NIAC
        integer(i4),POINTER  :: NIABCGS
        integer(i4),POINTER  :: NIAPC,NJAPC,NNZAPC
        real(sp),POINTER  :: HCLOSEPCGU,RCLOSEPCGU
        real(sp),POINTER  :: RELAXPCGU
        real(dp), POINTER, DIMENSION(:)     :: DSCALE
        real(dp), POINTER, DIMENSION(:)     :: DSCALE2
        integer(i4),          POINTER, DIMENSION(:)     :: IAPC
        integer(i4),          POINTER, DIMENSION(:)     :: JAPC
        real(dp), POINTER, DIMENSION(:)     :: APC
        integer(i4),          POINTER, DIMENSION(:)     :: LORDER
        integer(i4),          POINTER, DIMENSION(:)     :: IORDER
        integer(i4),          POINTER, DIMENSION(:)     :: IARO
        integer(i4),          POINTER, DIMENSION(:)     :: JARO
        real(dp), POINTER, DIMENSION(:)     :: ARO
        !         WORKING ARRAYS        
        integer(i4),         POINTER, DIMENSION(:)      :: IWC
        real(dp), POINTER, DIMENSION(:)     :: WC
        integer(i4),         POINTER, DIMENSION(:)      :: ID
        real(dp), POINTER, DIMENSION(:)     :: XC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: DC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: PC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: QC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: ZC
        !         BICGSTAB WORKING ARRAYS
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: TC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: VC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: DHATC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: PHATC
        DOUBLEPRECISION, POINTER, DIMENSION(:)      :: QHATC
        !         POINTERS FOR USE WITH BOTH ORIGINAL, RCM, AND MINIMUM DEGREE ORDERINGS
        integer(i4),          POINTER, DIMENSION(:)     :: IA0
        integer(i4),          POINTER, DIMENSION(:)     :: JA0
        real(dp), POINTER, DIMENSION(:)     :: A0
    END TYPE
    TYPE(PCGUTYPE), SAVE ::PCGUDAT(10)
end MODULE PCGUMODULE

!------------------------------------------------------------------
MODULE SWFBCMODULE
    use KindParameters
    implicit none
    integer(i4),SAVE,POINTER  ::NSWBC,NCRD,NZDG,NVSWBC,ISWBCCB    !NDRAIN,MXDRN,NDRNVL,IDRNCB,IPRDRN
    integer(i4),SAVE, DIMENSION(:), ALLOCATABLE     :: ISWBC
    real(dp), SAVE, DIMENSION(:,:), ALLOCATABLE     :: VSWBC
    real(dp), SAVE, DIMENSION(:), ALLOCATABLE     :: QSWBC
        
    real(dp) :: GRAV_SWF
END MODULE SWFBCMODULE
