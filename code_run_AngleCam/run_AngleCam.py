# Code Description:

# In this code, the parameters and their explanations are as follows:

# - `directory`: The directory path where the image data is located. It can be set to the desired directory path.
# - `model_path`: The file path to the model weights file (.hdf5) that will be loaded for inference.
# - `imagepaths`: A list of image file paths to be processed. These files should be located in the "wildcam12_test" folder within the directory.

# The code performs the following steps:

# 1. Loads the model weights using `keras.models.load_model`.
# 2. Defines a function `get_image_taken_datetime` to extract the image taken date and time from the metadata using PIL.
# 3. Loads the image data from the specified folder using `glob.glob` and stores the image file paths in `imagepaths`.
# 4. Processes and resizes the images using OpenCV, scaling the pixel values between 0 and 1.
# 5. Applies the loaded model to the processed images and saves the predictions to a CSV file.
# 6. Extracts the date and time metadata from the images using the `get_image_taken_datetime` function.
# 7. Calculates the average angle from the prediction dataset.
# 8. Creates a DataFrame (`df`) containing the extracted dates, average angles, and file names.
# 9. Saves the DataFrame to a CSV file.
# 10. Plots the average leaf angles over time using Matplotlib and saves the plot as an image.

# Please make sure to modify the relevant parameters and file paths according to your specific setup.

import tensorflow as tf
import keras
import pandas as pd
import glob
import os
os.environ["KMP_DUPLICATE_LIB_OK"]="TRUE"
import cv2
import matplotlib.pyplot as plt
import numpy as np
#from PIL import Image
#from PIL.ExifTags import TAGS

# Set the directory where the image data are located
directory = os.getcwd()
print("Working directory: " + str(directory))

# Load the model weights
model_path = "G:/My Drive/1_coding/anglecam/1_AngleCam_application/v2/AngleCam_efficientnet_V2L_14-03-2023.hdf5"
model = keras.models.load_model(model_path)

# Check if the model is loaded correctly
if model:
    print("Model loaded successfully.")
else:
    print("Failed to load the model.")


# Set GPU settings if running TensorFlow on GPU
gpus = tf.config.experimental.list_physical_devices('GPU')
if gpus:
    tf.config.experimental.set_memory_growth(gpus[0], True)

# Load the image data
imagepaths = glob.glob(os.path.join(directory, "example_dataset_timeseries_tilia_cordata", "*.png"))
print("Taking following imagepath: " + str(imagepaths))
print(f"{len(imagepaths)} images found.")

# Process and resize the images
processed_images = []
for imagepath in imagepaths:
    image = cv2.imread(imagepath)
    resized_image = cv2.resize(image, (600, 600))
    processed_image = resized_image.astype(np.float32) / 255.0
    processed_images.append(processed_image)


imgdataset = tf.data.Dataset.from_tensor_slices(processed_images)

# Apply the model to the imagery and write output to file
pred_imgdataset = model.predict(imgdataset.batch(1))
pred_imgdataset = pred_imgdataset / 10
pred_imgdataset = pd.DataFrame(pred_imgdataset, index=[os.path.basename(path) for path in imagepaths])
pred_imgdataset.to_csv(os.path.join(directory, "AngleCam_prediction_table.csv"))

print("Column titles in pred_imgdataset:")
print(pred_imgdataset.columns)

# Extract the date and time metadata from the images
file_names = []
for imagepath in imagepaths:
    file_names.append(os.path.basename(imagepath))

dates = []
for filename in file_names:
    dates.append(pd.to_datetime(filename[14:33], format="%Y-%m-%d_%H-%M-%S", utc=True))


# Convert filenames to datetime objects
dates = [datetime.datetime.strptime(filename[-19:-9], "%Y-%m-%d") for filename in filenames]

def mean_angle(row):
    pred = row.iloc[1:]
    length = len(pred)
    angles = np.linspace(0, 90, num=length)
    result = np.sum(pred * angles)
    return result

avg_angle = pred_imgdataset.apply(mean_angle, axis=1)

print("Average angle: " + str(avg_angle))
data = {"dates": dates, "avg_angle": avg_angle, "file_name": file_names}
df = pd.DataFrame(data)

# Save the data to a CSV file
df.to_csv(os.path.join(directory, "leaf_angle_data.csv"), index=False)

# Plot average leaf angles over time
plt.scatter(dates, avg_angle)
plt.xlabel("Time (CEST)")
plt.ylabel("Average leaf angle [deg]")
plt.title("Average Leaf Angle over Time")
plt.savefig("leaf_angle_plot.png")

