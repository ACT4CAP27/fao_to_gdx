from accli import WKubeTask

task = WKubeTask(
    name="Fao 2 gdx",
    job_folder='./',
    base_stack='GAMS40_1__R4_0',
    command="Rscript main.R",
    required_cores=1,
    required_ram=1024*1024*512,
    required_storage_local=1024*1024*2,
    required_storage_workflow=1024*1024,
    timeout=60*60,
    conf={
        "input_mappings": "acc://act4cap27/temp_rds/:/code/inputs/",
        "output_mappings": "/code/outputs/:acc://out"
    }
)

# python -m accli login --webcli=https://localhost:8080 -s=http://web_be:8000
# python -m accli dispatch bightspace task -s=http://web_be:8000