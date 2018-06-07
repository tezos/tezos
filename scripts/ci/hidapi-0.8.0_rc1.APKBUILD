# Contributor: Sören Tempel <soeren+alpine@soeren-tempel.net>
# Maintainer:
pkgname=hidapi
pkgver=0.8.0_rc1
_relver="$(echo "$pkgver" | sed s/_/-/)"
pkgrel=0
pkgdesc="Simple library for communicating with USB and Bluetooth HID devices"
url="http://www.signal11.us/oss/hidapi/"
arch="all"
license="custom"
depends=""
depends_dev=""
options="!check"
makedepends="libusb-dev libtool eudev-dev linux-headers autoconf automake"
install=""
subpackages="$pkgname-dev $pkgname-doc"
source="$pkgname-$pkgver.tar.gz::https://github.com/signal11/$pkgname/archive/$pkgname-${_relver}.tar.gz"
builddir="$srcdir/$pkgname-$pkgname-$_relver"

prepare() {
	default_prepare
	cd "$builddir"
	./bootstrap
}

build() {
	cd "$builddir"
	./configure \
		--build=$CBUILD \
		--host=$CHOST \
		--prefix=/usr \
		--sysconfdir=/etc \
		--mandir=/usr/share/man \
		--localstatedir=/var
	make
}

package() {
	cd "$builddir"
	make DESTDIR="$pkgdir" install

	mkdir -p "$pkgdir"/usr/share/licenses/$pkgname
	mv "$pkgdir"/usr/share/doc/$pkgname/LICENSE* \
		"$pkgdir"/usr/share/licenses/$pkgname
}

sha512sums="4529d74e715c47d788b533d94bf0ef35fa773240c9a59558d30c5ecc78cf46961de368f9385f5d84d378eaf8d4e941d553341e839674e562ccfcf52726620a65  hidapi-0.8.0_rc1.tar.gz"
