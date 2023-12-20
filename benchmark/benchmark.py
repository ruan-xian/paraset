import subprocess
import csv

def run_all_cvp(command, num_runs):
    header = ["run","c", "v", "p", "real_time"]
    # all_c = [10, 50, 100, 150, 200]
    # all_v = [2,3,5]
    # all_p = [2,3,5]
    all_c = [100]
    all_v = [5]
    all_p = [5]
    all_data = []
    # build
    subprocess.run('stack build', shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # dummy run
    subprocess.run("time stack exec -- paraset-exe 12 3 4 +RTS -l -N4", shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    print("run", "c", "v", "p", "real_time")
    for c in all_c:
        for v in all_v:
            for p in all_p:
                for run in range(0,num_runs):
                    cur_data = {"run": run, "c":c, "v":v, "p":p}
                    cur_process = subprocess.run(command.format(c,v,p), shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                    runtime = cur_process.stderr
                    cur_data["real_time"] = get_real_time(runtime)
                    all_data.append(cur_data)
                    print(run, c, v, p, cur_data["real_time"])
    return all_data, header

def run_all_cores(command ,num_runs):
    header = ["run", "HEC", "real_time"]
    all_cores = [1,2,4,6,8]
    all_data = []
    # build
    subprocess.run('stack build', shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # dummy run
    subprocess.run("time stack exec -- paraset-exe -s -r 69 -v 3 12 3 4 +RTS -l -H4G -N4", shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    print("run","HEC", "real_time")
    for cur_cores in all_cores:
        for run in range(0,num_runs):
            cur_data = {"run":run, "HEC":cur_cores}
            cur_process = subprocess.run(command.format(cur_cores), shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            runtime = cur_process.stderr
            cur_data["real_time"] = get_real_time(runtime)
            all_data.append(cur_data)
            print(run, cur_cores, cur_data["real_time"])
    return all_data, header

def get_real_time(runtime):
    real_time = runtime.split("real")[1].split("user")[0].split()[0]
    minutes = real_time.split("m")[0]
    seconds = real_time.split("m")[1].split("s")[0]
    return float(minutes) * 60 + float(seconds)


def write_to_csv(file_name, data, header):
    with open(file_name, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=header)
        writer.writeheader()
        writer.writerows(data)

if __name__ == "__main__":
    commands_dict = {
        "v1_cvp":"time stack exec -- paraset-exe  -s -r 69 -v 1 {0} {1} {2} +RTS -l -N4",
        "v3_cvp":"time stack exec -- paraset-exe -s -r 69 -v 3 {0} {1} {2} +RTS -l -H4G -N4",
        "v3_cores":"time stack exec -- paraset-exe -s -r 69 -v 3 150 5 5 +RTS -l -H4G -N{0}",
        "v6_cores":"time stack exec -- paraset-exe -s -r 69 -v 6 150 5 5 +RTS -l -H4G -N{0}"
    }

    # v1_cvp_data, header = run_all_cvp(commands_dict["v1_cvp"], 5)
    v3_core_data, header = run_all_cores(commands_dict["v3_cores"], 5)

    # write_to_csv("v1_seq_data.csv", v1_seq_data, header)