from composition_root import CompositionRoot
from collections import namedtuple
from esp32 import Esp32

# First we create a composition root subclass with providers for each type of sensor
# Then generate csv files with the input data for each sensor
#   - because the input to all pipelines in all datas is the same within the same sensor!!!
# The first line is the names of the variables that the sensor provides
# Each following line is a single sample
# mpu6050.csv
# AcX, AcY, AcZ, Tmp, GyX, GyY, GyZ
# ..., ..., ..., ..., ..., ..., ...
# ..., ..., ..., ..., ..., ..., ...
# ..., ..., ..., ..., ..., ..., ...
# ..., ..., ..., ..., ..., ..., ...

class DriverStub():

    def __init__(self, file_name: str):
        self.samples = []

        with open(file_name, "r") as file:
            types = " ".join(file.readline().split(", "))
            self.Tuple = namedtuple("Stub", types)

            self.samples = [
                [float(value) for value in line.split(", ")]
                for line in file.readlines()
            ]

    def sample(self):
        values = self.samples.pop(0)
        return self.Tuple(*values)

class ChannelStub:

    def __init__(self, file_name: str):
        self.file_name = file_name

    def send(self, data: bytes):
        with open(self.file_name, "a") as file:
            lst = list(data)
            file.write("{}\n".format(", ".join(map(str, lst))))
            file.flush()
    
    def receive(self) -> bytes:
        return None

class DeviceStub(Esp32):

    def _input_loop(self, thread):
        lines = 0
        with open("adc.csv", "r") as adc:
            for line in adc:
                lines += 1
            
        for _ in range(lines):
            for sensor_name in self._sensors:
                self._sensors[sensor_name].signal("signal")

class CustomCompositionRoot(CompositionRoot):
    def provide_device_esp32(self):
        return DeviceStub()
    
    def provide_driver_adc(self, *pins):
        return DriverStub("adc.csv")

    def provide_driver_mpu6050(self):
        return DriverStub("mpu6050.csv")

    def provide_driver_mag3110(self):
        return DriverStub("mag3110.csv")

    def provide_driver_fbm320(self):
        return DriverStub("fbm320.csv")

    def provide_driver_hts221(self):
        return DriverStub("hts221.csv")

    def provide_driver_bh1750(self):
        return DriverStub("bh1750fvi.csv")
    
    def make_channel(self, identifier: str):
        return ChannelStub(identifier + ".csv")

CustomCompositionRoot().provide_esp32().run()
