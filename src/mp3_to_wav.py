# Import packages
import os
from pydub import AudioSegment # sudo apt-get install ffmpeg
import time

# Change working directory to mp3 files folder
os.chdir("../data/xeno-canto/mp3")

# Save mp3 files names
audio_files = os.listdir()

# Initialise variables
count = 0
start = time.time()

# Iterate over files in the folder
print('READING AUDIO DATA...')

for file in audio_files:
    # Splitting the file into the name and the extension
    name, ext = os.path.splitext(file)
    if ext == ".mp3":
        try:
            mp3_sound = AudioSegment.from_file(file).set_channels(1)
            mp3_sound.export("../wav/{0}.wav".format(name), format="wav")
            os.remove(file)
            count += 1
        except:
            print('An exception occurred with file {0}.mp3'.format(name))

print('DONE! CONVERTED', str(count), '.mp3 AUDIO FILES TO .wav.')
print('Time', int((time.time() - start)), 'SECONDS')

# Delete mp3 folder
os.rmdir("/home/esther/Documentos/main/data/xeno-canto/mp3")

# Change working directory to src files folder
os.chdir("../../../src")
