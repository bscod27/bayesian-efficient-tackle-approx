from moviepy.editor import VideoFileClip
import imageio

input_file = '../data/example_video.mov'
output_file = '../results/example_play.gif'

video_clip = VideoFileClip(input_file)
resized_clip = video_clip.resize(newsize=(1000, 500))
resized_clip.write_gif(output_file, fps=10)
