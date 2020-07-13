import os, glob
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image
jpeg_list = sorted(set([string for string in glob.glob('/Users/bhargaa/Documents/Experiments/Subclone-Size-Distribution/data/processed-acqusitions/**/*.jpg')]) - set([string for string in glob.glob('/Users/bhargaa/Documents/Experiments/Subclone-Size-Distribution/data/processed-acqusitions/**/*-Color-Code.jpg')]))
color_code_list = sorted(set([string for string in glob.glob('/Users/bhargaa/Documents/Experiments/Subclone-Size-Distribution/data/processed-acqusitions/**/*-Color-Code.jpg')]))
titles = sorted([titles.split('/',1)[0] for titles in [string.split('/processed-acqusitions/',1)[1] for string in jpeg_list]])
dictionary = dict(zip(titles, zip(jpeg_list, color_code_list)))
for k,v in dictionary.items():
    left_image = np.array(Image.open(v[0]))
    right_image = np.array(Image.open(v[1]))
    data = np.concatenate((left_image, right_image), axis = 1)
    final = Image.fromarray(data)
    fig = plt.figure(dpi = 900)
    plt.imshow(final)
    plt.savefig('/Users/bhargaa/Desktop/Image-Pairs-Erik/%s.pdf' % k, bbox_inches='tight')
    fig.clear()
    plt.close()
