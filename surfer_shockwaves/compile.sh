cargo build --target wasm32-unknown-unknown

# optionally:
if [ "$1" = "linux" ]; then
  cp target/wasm32-unknown-unknown/debug/surfer_shockwaves.wasm ~/.config/surfer/translators
  echo "copied to Linux folder"
fi
if [ "$1" = "windows" ]; then
  cp target/wasm32-unknown-unknown/debug/surfer_shockwaves.wasm ~/AppData/Roaming/surfer-project/surfer/data/translators
  echo "copied to Windows folder"
fi

echo "[DONE]"
sleep 5