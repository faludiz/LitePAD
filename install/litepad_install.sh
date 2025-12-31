rm -r -f litepad

mkdir litepad
mkdir litepad/languages

cp -u ../bin/litepad litepad
cp -u -r ../bin/languages/*.* litepad/languages
cp -u ../source/litepad_96.png litepad
cp -u ../LICENSE litepad
cp -u ../README.md litepad

tar -czvf ./linux/litepad_25.12.0.8_amd64.tar.gz litepad
rm -r -f litepad
