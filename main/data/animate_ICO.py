from PIL import Image
import math
import os
import shutil
from apng import APNG

input_file = "main/www/ICO.png"
output_file = "main/www/animated_ICO.png"
num_frames = 40       # Total number of frames in the animation
amplitude = 0.04      # Subtle effect


# Open the original image and get its dimensions
original = Image.open(input_file).convert("RGBA")
width, height = original.size
frame_files = []

# Create a temporary directory for frames
os.makedirs("frames_temp", exist_ok=True)

for i in range(num_frames):
    t = 2 * math.pi * i / num_frames
    scale = 1.0 + amplitude * math.sin(t)
    
    new_width = int(width * scale)
    new_height = int(height * scale)
    
    resized = original.resize((new_width, new_height), resample=Image.Resampling.LANCZOS)
    
    # Create a new transparent canvas of the original image size
    frame = Image.new("RGBA", (width, height), (255, 255, 255, 0))
    paste_x = (width - new_width) // 2
    paste_y = (height - new_height) // 2
    frame.paste(resized, (paste_x, paste_y), resized)
    
    frame_path = f"frames_temp/frame_{i:02d}.png"
    frame.save(frame_path)
    frame_files.append(frame_path)

# Combine frames into an animated PNG using the apng library
APNG.from_files(frame_files, delay=50).save(output_file)
print(f"Saved animated PNG as {output_file}")

shutil.rmtree("frames_temp")