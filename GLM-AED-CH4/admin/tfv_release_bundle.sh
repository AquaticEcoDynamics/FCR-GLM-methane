#!/bin/sh
#
# tfv_release_bundle.sh
#
# Script to bundle up TuflowFV with libaed-* and gotm as a TuflowFV.tgz

CWD=`pwd`

# keep the default as ifort in the common 'Tuflow' folder
export FC=ifort

export TFV_VRS=""
export WITH_TAG=""
LFV_VRS=`grep FV_AED_VERS libaed-fv/src/fv_aed.F90 | grep -w define | cut -f2 -d\"`

while [ $# -gt 0 ] ; do
  case $1 in
    --do-tagging)
      export DO_TAG=true
      export WITH_TAG="$1"
      if [ $# -gt 1 ] ; then
        if [ "${2#--*}" = "$2" ] ; then
           TFV_VRS="$2"
           shift
        fi
      fi
      ;;
    *)
      ;;
  esac
  shift
done

# start by cleaning the respositories
# this would be done by "admin/libfv_release_bundle.sh"
#echo cleaning repositories ....
#./clean.sh > /dev/null 2>&1
#echo ... done
#
#if [ -d tttt ] ; then
#   /bin/rm -rf tttt
#fi


if [ "$TFV_VRS" = "" ] ; then
  TFV_VRS=`date +%Y%m%d`
fi
if [ "${DO_TAG}" = "true" ] ; then
  export PRTAG="TFV_${TFV_VRS}"
fi

if [ "${PRTAG}" = "" ] ; then
  ./admin/libfv_release_bundle.sh ${WITH_TAG}
else
  ./admin/libfv_release_bundle.sh ${WITH_TAG} --tag "${PRTAG}"
fi
mkdir tttt

tar cf - gotm-git | (cd tttt; tar xf -)
/bin/rm -rf tttt/gotm-git/.git*

tar cf - tuflowfv-svn | (cd tttt; tar xf -)
/bin/rm -rf tttt/tuflowfv-svn/.git*

tar cf - admin build_tuflow.sh READ* fetch* | (cd tttt; tar xf -)
zcat binaries/sources/libaed_fv-${LFV_VRS}.tar.gz | (cd tttt; tar xf -)

if [ `find tttt/ -name .DS_Store` ] ; then
  /bin/rm `find tttt/ -name .DS_Store`
fi
if [ `find tttt/ -name ._.DS_Store` ] ; then
  /bin/rm `find tttt/ -name ._.DS_Store`
fi

mv tttt TuflowFV-${TFV_VRS}
tar czf TuflowFV-${TFV_VRS}.tar.gz TuflowFV-${TFV_VRS}
zip -rq TuflowFV-${TFV_VRS}.zip TuflowFV-${TFV_VRS}
mv TuflowFV-${TFV_VRS} tttt

if [ -d libaed-dev ] ; then
  zcat binaries/sources/libaed_fv_Plus-${LFV_VRS}.tar.gz | (cd tttt; tar xf -)

  if [ `find tttt/ -name .DS_Store` ] ; then
    /bin/rm `find tttt/ -name .DS_Store`
  fi
  if [ `find tttt/ -name ._.DS_Store` ] ; then
    /bin/rm `find tttt/ -name ._.DS_Store`
  fi
  mv tttt TuflowFV_Plus-${TFV_VRS}
  tar czf TuflowFV_Plus-${TFV_VRS}.tar.gz TuflowFV_Plus-${TFV_VRS}
  zip -rq TuflowFV_Plus-${TFV_VRS}.zip TuflowFV_Plus-${TFV_VRS}
  mv TuflowFV_Plus-${TFV_VRS} tttt
fi

/bin/rm -rf tttt
mkdir -p binaries/sources
mv TuflowFV-${TFV_VRS}.tar.gz TuflowFV-${TFV_VRS}.zip binaries/sources
if [ -d libaed-dev ] ; then
  mv TuflowFV_Plus-${TFV_VRS}.tar.gz TuflowFV_Plus-${TFV_VRS}.zip binaries/sources
fi

exit 0
