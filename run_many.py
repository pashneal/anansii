import subprocess

def run_command(command):
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    # write to file called output.txt
    # only if the output doesn't contain "afterallmoves"
    if "afterallmoves" not in stdout.decode():
        with open("output.txt", "a") as f:
            f.write(stdout.decode())
            f.write(stderr.decode())


    

if __name__ == "__main__":
    print("hello")
    for i in range(1000):
        run_command("cargo test -- sanity_check")
