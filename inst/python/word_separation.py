import os
import cv2
import numpy as np
import matplotlib.pyplot as plt

def separate_word(file_name, ret="image"):
  # Set image path
  file_name = os.path.join("images", file_name)
  
  # Read Input image
  input_image = cv2.imread(os.path.join(file_name))
  gray = cv2.cvtColor(input_image, cv2.COLOR_BGR2GRAY)
  
  # Threshold via Otsu:
  thresh_value, binary_image = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)
  binary_image = cv2.medianBlur(binary_image, 5)
  
  # TODO: Split image into two now?? apply separately and combine?
  thresh = cv2.adaptiveThreshold(binary_image, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 11, 8)
  kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (5, 5))
  dilate = cv2.dilate(thresh, kernel, iterations=4)
  
  # Deep copy for results:
  contours, hierarchy = cv2.findContours(dilate, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE) # get contours
  input_copy = input_image.copy()
  
  # Look for the outer bounding boxes (no children):
  for _, c in enumerate(contours):
  
      # Get the bounding rectangle of the current contour:
      boundRect = cv2.boundingRect(c)
  
      # Get the bounding rectangle data:
      rectX = boundRect[0]
      rectY = boundRect[1]
      rectWidth = boundRect[2]
      rectHeight = boundRect[3]
  
      # Estimate the bounding rect area:
      rectArea = rectWidth * rectHeight
  
      # Draw bounding box:
      color = (0, 255, 0)
      
      area = cv2.contourArea(c)
      if area > 1000:
          cv2.rectangle(input_copy, (int(rectX), int(rectY)),
                        (int(rectX + rectWidth), int(rectY + rectHeight)), color, 2)
  
      # Crop bounding box:dd
      currentCrop = input_image[rectY:rectY+rectHeight,rectX:rectX+rectWidth]
          
  # load image using cv2....and do processing.
  final_im = cv2.cvtColor(input_copy, cv2.COLOR_BGR2RGB)
  
  if ret == "contours":
    return contours
  
  return(final_im)
  
  
def show_image(im):
  # load image using cv2....and do processing.
  plt.imshow(im)
  # as opencv loads in BGR format by default, we want to show it in RGB.
  plt.tight_layout()
  plt.show()

  
def concat_images(im1, im2):
  v_img = cv2.vconcat([im1, im2])
  
  return(v_img)

def annotate_image(file_name, contours):
  # Read Input image
  if type(file_name) == str:
    # Set image path
    file_name = os.path.join("images", file_name)
    input_copy = cv2.imread(os.path.join(file_name))
  else:
    input_copy = file_name
  
  # Look for the outer bounding boxes (no children):
  for _, c in enumerate(contours):
    area = cv2.contourArea(c)
    if area > 1000:
        
      # Get the bounding rectangle of the current contour:
      boundRect = cv2.boundingRect(c)
  
      # Get the bounding rectangle data:
      rectX = boundRect[0]
      rectY = boundRect[1]
      rectWidth = boundRect[2]
      rectHeight = boundRect[3]
  
      # Estimate the bounding rect area:
      rectArea = rectWidth * rectHeight
  
      # Draw bounding box:
      color = (0, 255, 0)
      
      cv2.rectangle(input_copy, (int(rectX), int(rectY)),
                    (int(rectX + rectWidth), int(rectY + rectHeight)), color, 2)
  #    print(_)
  #    print(c)
  # Get top left corner of every corner
  vals = [[0, 0, 0] for c in contours]
  for i, c in enumerate(contours):
      vals[i][0] = min([x[0][0] for x in c])
      vals[i][1] = min([x[0][1] for x in c])
      if vals[i][1] < 100:
          vals[i][2] = 0
      else:
          vals[i][2] = 1
  
  res_vals = sorted(vals, key = lambda x: x[2] * 1000000 + x[0])

  font = cv2.FONT_HERSHEY_SIMPLEX

  for i, res in enumerate(res_vals):
      y = 50 * res[2] + res[1] + 75
      cv2.putText(input_copy, str(i), (res[0], y), font, 3, (255, 0, 0), 2, cv2.LINE_AA)
  
  plt.imshow(cv2.cvtColor(input_copy, cv2.COLOR_BGR2RGB))
  plt.show()

