import sys

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Expected a file as argument.")
        exit(1)

    with open(sys.argv[1], mode='rb') as file:
        data_read = file.read(4)
        addr = 0

        while data_read != b'':
            i = int.from_bytes(data_read, "little")
            r = f"{i:0>32b}"
            print(f"0x{addr:0>8x} {r[:-4]}{r[-4:]}")
            data_read = file.read(4)
            addr += 1
