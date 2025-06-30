import os
import cv2
import numpy as np
import subprocess
from tqdm import tqdm

INPUT_DIR = r"J:\NF2503\NF2503\Bender Recovery\NF2402_URIALBEX02002\Raw\Camera_Rayfin\Videos"
OUTPUT_DIR = r"\\primnoa.cels.uri.edu\Video_and_Imagery\Video_Archive\MDBC\NF2503_URIALBEX061_T100_Viosca_Trial\Repairing_Tests\SH60_Videos\Repaired"
LOG_FILE = "processing_log.txt"

os.makedirs(OUTPUT_DIR, exist_ok=True)
log_entries = []

def sample_video_frames(video_path, sample_rate=1):
    cap = cv2.VideoCapture(video_path)
    fps = cap.get(cv2.CAP_PROP_FPS)
    frame_interval = int(fps * sample_rate)

    brightness_vals = []
    rgb_vals = []

    frame_count = 0
    while True:
        ret, frame = cap.read()
        if not ret:
            break
        if frame_count % frame_interval == 0:
            hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
            rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)

            v = hsv[..., 2]
            brightness = np.mean(v)
            rgb_mean = np.mean(rgb, axis=(0, 1))  # R, G, B

            # Filter out extreme frames
            if 5 < brightness < 250 and all(5 < ch < 250 for ch in rgb_mean):
                brightness_vals.append(brightness)
                rgb_vals.append(rgb_mean)

        frame_count += 1

    cap.release()
    return brightness_vals, rgb_vals

# Step 1: Scan and analyze
all_brightness = []
all_rgb = []
video_files = [f for f in os.listdir(INPUT_DIR) if f.endswith(".mp4")]

print("🔍 Analyzing video files...")
for filename in tqdm(video_files, desc="Analyzing", unit="video"):
    path = os.path.join(INPUT_DIR, filename)
    b_vals, rgb_vals = sample_video_frames(path)
    if b_vals and rgb_vals:
        all_brightness.extend(b_vals)
        all_rgb.extend(rgb_vals)
    else:
        log_entries.append(f"[SKIPPED] {filename} - no valid frames found.")

if not all_brightness or not all_rgb:
    raise RuntimeError("No valid frames found in any videos. Exiting.")

global_brightness = np.mean(all_brightness)
global_rgb = np.mean(all_rgb, axis=0)

log_entries.append(f"\n[GLOBAL AVERAGES] Brightness: {global_brightness:.2f}, RGB: {global_rgb.tolist()}")

def apply_corrections(input_path, output_path, target_brightness, target_rgb, filename):
    b_vals, rgb_vals = sample_video_frames(input_path)
    if not b_vals or not rgb_vals:
        log_entries.append(f"[SKIPPED] {filename} - no valid frames.")
        return

    current_brightness = np.mean(b_vals)
    current_rgb = np.mean(rgb_vals, axis=0)

    brightness_delta = (target_brightness - current_brightness) / 255.0
    rgb_ratios = target_rgb / current_rgb
    rgb_ratios = np.clip(rgb_ratios, 0.5, 1.5)

    eq_filter = f"eq=brightness={brightness_delta:.4f}"
    colorbalance_filter = (
        f"colorchannelmixer=rr={rgb_ratios[0]:.4f}:gg={rgb_ratios[1]:.4f}:bb={rgb_ratios[2]:.4f}"
    )
    vf = f"{eq_filter},{colorbalance_filter}"

    command = [
        "ffmpeg",
        "-i", input_path,
        "-vf", vf,
        "-c:v", "libx264",
        "-crf", "18",
        "-preset", "fast",
        "-c:a", "copy",
        output_path
    ]

    log_entries.append(
        f"[PROCESSED] {filename}\n"
        f"    Original Brightness: {current_brightness:.2f}\n"
        f"    Original RGB: {current_rgb.tolist()}\n"
        f"    Brightness Δ: {brightness_delta:.4f}\n"
        f"    RGB Ratios: {rgb_ratios.tolist()}"
    )

    subprocess.run(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

print("\n🎬 Processing and saving videos...")
for filename in tqdm(video_files, desc="Processing", unit="video"):
    input_path = os.path.join(INPUT_DIR, filename)
    output_path = os.path.join(OUTPUT_DIR, filename)
    apply_corrections(input_path, output_path, global_brightness, global_rgb, filename)

# Save log
with open(LOG_FILE, "w") as f:
    f.write("\n".join(log_entries))

print("\n✅ All videos processed. Log written to:", LOG_FILE)
