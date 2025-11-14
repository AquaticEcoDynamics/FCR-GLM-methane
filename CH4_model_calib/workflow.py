import os
import sys
import shutil
sys.path.insert(0,".")
import pyemu

def prep_for_glm(org_d="model_files"):
    """prep for an ies run
    """

    # the location of the ies prep'd model+pest files
    t_d = "template"
    if os.path.exists(t_d):
        shutil.rmtree(t_d)
    shutil.copytree(org_d,t_d)

    pst_name = os.path.join(t_d, "glm3.pst")
    if not os.path.exists(pst_name):
        raise Exception("PST file is missing.")
    pst = pyemu.Pst(pst_name)

    #check that ies likes the inputs
    #pst.pestpp_options["debug_parse_only"] = True
    #pyemu.os_utils.run("pestpp-glm {0}".format(os.path.split(pst_name)[1]),cwd=t_d)

    # now do the ever important test run
    #pst.pestpp_options["debug_parse_only"] = False
    #pst.control_data.noptmax = 0
    #pst.write(pst_name,version=2)
    #pyemu.os_utils.run("pestpp-glm {0}".format(os.path.split(pst_name)[1]),cwd=t_d)

    #now save for actually ies run
    #pst.control_data.noptmax = 50
    #pst.write(pst_name,version=2)


def run_glm_locally(t_d="template",m_d="master_ies",num_workers=10):
    """run pestpp-ies locally in parallel

    Note: set the number of workers to appropriate for what your machine can handle!
    """
    pyemu.os_utils.start_workers(t_d,"pestpp-glm","glm3.pst",num_workers=num_workers,
                                 worker_root=".",master_dir=m_d)

if __name__ == "__main__":
    prep_for_glm()
    run_glm_locally()
