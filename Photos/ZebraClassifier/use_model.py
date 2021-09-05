import numpy as np
import os
from matplotlib import pyplot as plt
import cv2
import random
import tensorflow as tf

from variables import FOLDERS, DIMENSIONS
IMG_SIZE = DIMENSIONS

print("Running: use_model.py at "+str(IMG_SIZE)+"x"+str(IMG_SIZE))

# 
path = "/Users/maxgotts/Desktop/MPALA/Photos/" #os.path.join(DATADIR) #input("Path to files: ")
for img in os.listdir(path):
    img_array = cv2.imread(os.path.join(path, img), cv2.IMREAD_GRAYSCALE)

import_images = []
image_filenames = os.listdir(path)

def do_import_images():
    for img in os.listdir(path):
        try :
            img_array = cv2.imread(os.path.join(path, img), cv2.IMREAD_GRAYSCALE)
            new_array = cv2.resize(img_array, (IMG_SIZE, IMG_SIZE))
            import_images.append(new_array)
        except Exception as e: pass

do_import_images()

import_images = np.array(import_images).reshape(-1, IMG_SIZE, IMG_SIZE)


# Normalize the images
import_images = import_images/255.0

# Reshape the images.
images = np.expand_dims(import_images, axis=3)

# Create a new model instance
model_json_file = open("classify_cnn.json","r")
model_json = model_json_file.read()
model = tf.keras.models.model_from_json(model_json)

# Load the previously saved weights
model.load_weights("classify_cnn.h5")

# Classify using model
classification = model.predict(images)
print(classification)