#!/usr/bin/env sh

APP="$1"

cat <<EOF > /tmp/focus_last.js
let windows = workspace.stackingOrder;
for (let i = windows.length - 1; i >= 0; i--) {
    let w = windows[i];
    if (w.resourceClass.toLowerCase().includes('$APP') || w.caption.toLowerCase().includes('$APP')) {
        workspace.activeWindow = w;
        break;
    }
}
EOF

# 2. Tell KWin to load the script (it returns a unique Script ID number)
SCRIPT_ID=$(qdbus6 org.kde.KWin /Scripting org.kde.kwin.Scripting.loadScript "/tmp/focus_last.js" "temp_focus_script2")

# 3. Tell KWin to run the script using that unique ID
qdbus6 org.kde.KWin "/Scripting/Script${SCRIPT_ID}" org.kde.kwin.Script.run

# 4. Clean up the script out of KWin's memory and disk
qdbus6 org.kde.KWin /Scripting org.kde.kwin.Scripting.unloadScript "temp_focus_script2"
rm /tmp/focus_last.js
