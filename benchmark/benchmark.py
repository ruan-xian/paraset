import subprocess
import csv

def run_seq_combos():
    all_c = [10, 50, 100, 150, 200]
    all_v = [2,3,5]
    all_p = [2,3,5]
    all_data = []
    subprocess.run('stack build', shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    for c in all_c:
        for v in all_v:
            for p in all_p:
                cur_data = {"c":c, "v":v, "p":p}
                cur_command = "time stack exec -- paraset-exe {0} {1} {2} +RTS -l -N4".format(c,v,p)
                cur_process = subprocess.run(cur_command, shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                runtime = cur_process.stderr
                real_time = runtime.split("real")[1].split("user")[0].split()[0]
                cur_data["real_time"] = real_time
                all_data.append(cur_data)
                print(c, v, p, real_time)
    return all_data


def write_to_csv(file_name, data):
    with open(file_name, mode='w', newline='', encoding='utf-8') as file:
        # Specify the fieldnames (header)
        header = ["c", "v", "p", "real_time"]
        writer = csv.DictWriter(file, fieldnames=header)

        # Write the header
        writer.writeheader()

        # Write the rows
        writer.writerows(data)

if __name__ == "__main__":
    seq_data = run_seq_combos()
    print("seq_data", run_seq_combos())

    write_to_csv("seq_data", seq_data)
    # c = 10
    # v = 2
    # p =2
    
    # subprocess.run('stack build', shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # test = "time stack exec -- paraset-exe {0} {1} {2} +RTS -l -N4".format(10,2,2)
    # cur_process = subprocess.run(test, shell=True, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # runtime = cur_process.stderr
    # print("sdfwwew", cur_process.stdout)
    # real_time = runtime.split("real")[1].split("user")[0].split()[0]
    # print("runtime", runtime)
