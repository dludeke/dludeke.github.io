#!/usr/bin/env python3
"""
Script to extract individual handshapes from a grid image of the manual alphabet.
This will divide the image into individual cells and save each handshape separately.
"""

from PIL import Image
import os

def extract_handshapes(input_path, output_dir, rows, cols, letter_names, crop_right_percent=0.25, crop_top_percent=0.1):
    """
    Extract handshapes from a grid image, removing letter labels.

    Args:
        input_path: Path to the input grid image
        output_dir: Directory to save extracted handshapes
        rows: Number of rows in the grid
        cols: Number of columns in the grid
        letter_names: List of letter names in reading order (left-to-right, top-to-bottom)
        crop_right_percent: Percentage to crop from the right side to remove letter labels (default 0.25 = 25%)
        crop_top_percent: Percentage to crop from the top to remove any top labels (default 0.1 = 10%)
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)

    # Load the image
    img = Image.open(input_path)
    width, height = img.size

    print(f"Image size: {width}x{height}")
    print(f"Grid: {rows} rows x {cols} columns")

    # Calculate cell dimensions
    cell_width = width // cols
    cell_height = height // rows

    print(f"Cell size: {cell_width}x{cell_height}")
    print(f"Cropping right {crop_right_percent*100}% and top {crop_top_percent*100}% from each cell")

    # Extract each cell
    index = 0
    for row in range(rows):
        for col in range(cols):
            if index >= len(letter_names):
                print(f"Warning: More cells than letter names provided")
                break

            # Calculate crop box for the full cell
            left = col * cell_width
            top = row * cell_height
            right = left + cell_width
            bottom = top + cell_height

            # Crop the cell
            cell = img.crop((left, top, right, bottom))

            # Further crop to remove the letter label (typically in upper right)
            # and any extra space at the top
            cell_w, cell_h = cell.size
            crop_right = int(cell_w * crop_right_percent)
            crop_top = int(cell_h * crop_top_percent)

            # Crop: remove from right and top
            cropped_cell = cell.crop((0, crop_top, cell_w - crop_right, cell_h))

            # Save the cell
            letter = letter_names[index]
            output_path = os.path.join(output_dir, f"{letter}.png")
            cropped_cell.save(output_path)

            print(f"Saved: {output_path} (row {row}, col {col})")
            index += 1

    print(f"\nExtracted {index} handshapes to {output_dir}")

if __name__ == "__main__":
    # Greek Sign Language alphabet
    # The letters in the image reading order (left to right, top to bottom)
    gsl_letters = [
        "alpha", "beta", "gamma", "delta",
        "epsilon", "zeta", "eta", "theta",
        "iota", "kappa", "lambda", "mu",
        "nu", "xi", "omicron", "pi",
        "rho", "sigma", "tau", "upsilon",
        "phi", "chi", "psi", "omega"
    ]

    input_image = "images/gsl_manual.png"
    output_directory = "images/gsl_handshapes"

    # The image appears to be a 6x4 grid (6 rows, 4 columns)
    # Crop 35% from right to remove blue letter labels, 10% from top
    extract_handshapes(input_image, output_directory, rows=6, cols=4, letter_names=gsl_letters,
                      crop_right_percent=0.35, crop_top_percent=0.10)

    print("\n" + "="*50)
    print("Extraction complete!")
    print("Please check the extracted images and verify they match the correct letters.")
    print("If any images are incorrect, you can manually rename them.")
