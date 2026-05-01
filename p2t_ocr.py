import sys
from pix2text import Pix2Text
import pyperclip # 用于写入剪贴板

def main():
    if len(sys.argv) < 2:
        return
    
    img_path = sys.argv[1]
    p2t = Pix2Text.from_config()

    result = p2t.recognize_formula(img_path)
    
    latex_result = f"${result}$"
    

    pyperclip.copy(latex_result)
    print(f"Success: {latex_result}")

if __name__ == "__main__":
    main()
