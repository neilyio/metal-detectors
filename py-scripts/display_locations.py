import typing as t
import json
from AppKit import NSScreen


def list_displays() -> t.List[dict]:
    screens = NSScreen.screens()
    display_info = []
    for index, screen in enumerate(screens):
        screen_id = screen.deviceDescription()["NSScreenNumber"]
        screen_size = screen.frame().size
        screen_origin = screen.frame().origin
        info = {
            "screen_id": screen_id,
            "width": screen_size.width,
            "height": screen_size.height,
            "origin_x": screen_origin.x,
            "origin_y": screen_origin.y,
            "index": index,
        }
        display_info.append(info)
    return display_info


if __name__ == "__main__":
    displays = list_displays()
    print(json.dumps(displays))
