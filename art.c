#define _GNU_SOURCE
#include "fb.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

enum {
    D_PLASMA, D_TUNNEL, D_ROTO, D_FIRE, D_META, D_STAR, D_TWIRL,
    D_JULIA, D_LORENZ, D_LSYS, D_IFS, D_WAVE,
    D_SCROLL, D_GLITCH, D_LIFE,
    D_XOR, D_MOIRE, D_RINGS, D_ROSE, D_LISSA, D_SIERP, D_RULE30,
    D_ANT, D_RAIN, D_SNOW, D_STATIC, D_VORONOI, D_FLOW, D_BOUNCE,
    D_FLAG, D_DOTTUN, D_BARS, D_KALEID, D_HOP, D_NOISE,
    NDEMO
};

static const char *dname[NDEMO] = {
    "plasma", "tunnel", "roto", "fire", "meta", "star", "twirl",
    "julia", "lorenz", "lsys", "ifs", "wave",
    "scroll", "glitch", "life",
    "xor", "moire", "rings", "rose", "lissa", "sierp", "rule30",
    "ant", "rain", "snow", "static", "voronoi", "flow", "bounce",
    "flag", "dottun", "bars", "kaleid", "hop", "noise"
};

static int mode, pause_on, want_quit, pkind;
static const char *pname[] = {"hue", "fire", "ice", "mono", "acid", "sunset", "gb", "vga"};
#define NPAL 8
static double t0, tsec;
static unsigned tick;
static struct termios oldt;
static int rawon;

static int8_t sint[256];
static uint16_t pal[256];
static uint16_t *tun_a, *tun_d; /* angle, dist packed */
static uint8_t fire[81][161];
static float star_x[400], star_y[400], star_z[400];
static float lor[800][3];
static int lor_n, lor_i;
static uint8_t life[2][80][120];
static int life_p;
static float wave[160], wave_v[160];
static int lsys_gen = 3;
static uint8_t rule30[320];
static int ant_x = 60, ant_y = 40, ant_d;
static uint8_t ant_g[80][120];
static float drop_x[80], drop_y[80];
static float snow_x[120], snow_y[120];
static float vx[12], vy[12], vdx[12], vdy[12];
static float hopx, hopy;
static int extra_inited;

static double now_s(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

static void pal_set(int kind)
{
    int i;
    pkind = ((kind % NPAL) + NPAL) % NPAL;
    for (i = 0; i < 256; i++) {
        int r, g, b, t = i;
        double a = i * 6.2832 / 256.0;
        switch (pkind) {
        case 1: /* fire */
            r = t;
            g = t / 2;
            b = t / 8;
            break;
        case 2: /* ice */
            r = 16;
            g = 40 + t / 2;
            b = 90 + t / 2;
            break;
        case 3: /* mono */
            r = g = b = t;
            break;
        case 4: /* acid */
            r = (int)(128 + 127 * sin(a * 2));
            g = t;
            b = 255 - t;
            break;
        case 5: /* sunset */
            r = 80 + t / 2;
            g = 20 + t / 4;
            b = t < 128 ? t / 3 : 180 - t / 3;
            break;
        case 6: /* gameboy */
            r = 15 + (t * 50) / 255;
            g = 56 + (t * 140) / 255;
            b = 15 + (t * 40) / 255;
            break;
        case 7: /* vga stripes */
            r = (t & 32) ? 220 : 40;
            g = (t & 64) ? 200 : 20;
            b = (t & 16) ? 255 : 80;
            break;
        default: /* hue */
            r = (int)(128 + 127 * sin(a));
            g = (int)(128 + 127 * sin(a + 2.1));
            b = (int)(128 + 127 * sin(a + 4.2));
            break;
        }
        if (r < 0)
            r = 0;
        if (g < 0)
            g = 0;
        if (b < 0)
            b = 0;
        pal[i] = rgb565(r, g, b);
    }
}

static void init_tables(void)
{
    int i, x, y, w, h;
    w = (int)FB_W;
    h = (int)FB_H;
    for (i = 0; i < 256; i++)
        sint[i] = (int8_t)(sin(i * 6.2832 / 256.0) * 127);
    pal_set(0);
    tun_a = malloc((size_t)w * h * 2);
    tun_d = malloc((size_t)w * h * 2);
    if (tun_a && tun_d) {
        for (y = 0; y < h; y++)
            for (x = 0; x < w; x++) {
                double dx = x - w / 2.0, dy = y - h / 2.0;
                double d = sqrt(dx * dx + dy * dy) + 0.01;
                tun_a[y * w + x] = (uint16_t)((atan2(dy, dx) + 3.1416) / 6.2832 * 65535);
                tun_d[y * w + x] = (uint16_t)((8000.0 / d));
            }
    }
    for (i = 0; i < 400; i++) {
        star_x[i] = (rand() % 400) - 200;
        star_y[i] = (rand() % 300) - 150;
        star_z[i] = (rand() % 400) + 8;
    }
    memset(fire, 0, sizeof fire);
    memset(life, 0, sizeof life);
    for (i = 0; i < 400; i++)
        life[0][rand() % 80][rand() % 120] = 1;
    for (i = 0; i < 160; i++) {
        wave[i] = 0;
        wave_v[i] = 0;
    }
    wave[80] = 40;
    {
        int k;
        for (k = 0; k < 320; k++)
            rule30[k] = 0;
        rule30[160] = 1;
        for (k = 0; k < 80; k++) {
            drop_x[k] = rand() % 480;
            drop_y[k] = rand() % 320;
        }
        for (k = 0; k < 120; k++) {
            snow_x[k] = rand() % 480;
            snow_y[k] = rand() % 320;
        }
        for (k = 0; k < 12; k++) {
            vx[k] = 40 + rand() % 400;
            vy[k] = 30 + rand() % 250;
            vdx[k] = 1.5f + (rand() % 20) / 8.0f;
            vdy[k] = 1.0f + (rand() % 16) / 8.0f;
        }
        hopx = 0;
        hopy = 0;
        extra_inited = 1;
    }
}

static void hud(void)
{
    char s[96];
    snprintf(s, sizeof s, "cArt [%d/%d] %s  pal:%s  c color  n/p  x",
             mode + 1, NDEMO, dname[mode], pname[pkind]);
    fill(0, 0, (int)FB_W, 10, 0);
    text(2, 2, s, rgb565(220, 230, 180));
}

/* ---------- demos ---------- */

static void d_plasma(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    int t = (int)(tsec * 40);
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        int sy = sint[(y + t) & 255] + sint[(y * 2 + t / 2) & 255];
        for (x = 0; x < w; x++) {
            int v = sy + sint[(x + t) & 255] + sint[(x + y + t) & 255];
            row[x] = pal[(v + 384) & 255];
        }
    }
}

static void d_tunnel(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    int t = (int)(tsec * 80);
    if (!tun_a)
        return;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        uint16_t *a = tun_a + y * w;
        uint16_t *d = tun_d + y * w;
        for (x = 0; x < w; x++) {
            int u = ((a[x] >> 8) + t) & 255;
            int v = ((d[x] >> 4) + t) & 255;
            row[x] = pal[(u ^ v) & 255];
        }
    }
}

static void d_roto(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    double a = tsec * 0.7;
    double ca = cos(a), sa = sin(a);
    double z = 1.2 + 0.4 * sin(tsec * 0.4);
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        double yy = (y - h / 2.0) * z;
        for (x = 0; x < w; x++) {
            double xx = (x - w / 2.0) * z;
            int u = (int)(xx * ca - yy * sa);
            int v = (int)(xx * sa + yy * ca);
            row[x] = ((u ^ v) & 16) ? pal[40] : pal[200];
        }
    }
}

static void d_fire(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    for (x = 0; x < 160; x++)
        fire[80][x] = (uint8_t)(180 + rand() % 70);
    for (y = 1; y < 80; y++)
        for (x = 1; x < 159; x++) {
            int v = (fire[y][x - 1] + fire[y][x] + fire[y][x + 1] + fire[y + 1][x]) / 4;
            v -= 2;
            fire[y - 1][x] = (uint8_t)(v < 0 ? 0 : v);
        }
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        int fy = y * 80 / h;
        if (fy > 79)
            fy = 79;
        for (x = 0; x < w; x++) {
            int fx = x * 160 / w;
            if (fx > 159)
                fx = 159;
            row[x] = pal[fire[fy][fx]];
        }
    }
}

static void d_meta(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, i, w = (int)FB_W, h = (int)FB_H;
    float bx[4], by[4];
    for (i = 0; i < 4; i++) {
        bx[i] = w / 2.0f + cosf((float)(tsec * (0.7 + i * 0.2) + i)) * (w * 0.28f);
        by[i] = h / 2.0f + sinf((float)(tsec * (0.9 + i * 0.15) + i * 2)) * (h * 0.28f);
    }
    for (y = 1; y < h; y += 1) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            float s = 0;
            for (i = 0; i < 4; i++) {
                float dx = x - bx[i], dy = y - by[i];
                s += 900.0f / (dx * dx + dy * dy + 8.0f);
            }
            row[x] = s > 1.2f ? pal[((int)(s * 40) + (int)(tsec * 20)) & 255] : 0;
        }
    }
}

static void d_star(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 400; i++) {
        int sx, sy;
        star_z[i] -= 3.2f;
        if (star_z[i] < 1) {
            star_x[i] = (rand() % 400) - 200;
            star_y[i] = (rand() % 300) - 150;
            star_z[i] = 400;
        }
        sx = (int)(w / 2 + star_x[i] * 80.0f / star_z[i]);
        sy = (int)(h / 2 + star_y[i] * 80.0f / star_z[i]);
        if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
            pix[sy * st + sx] = pal[200 - (int)star_z[i] / 3];
    }
}

static void d_twirl(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    double a = tsec * 0.5;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        double dy = y - h / 2.0;
        for (x = 0; x < w; x++) {
            double dx = x - w / 2.0;
            double r = sqrt(dx * dx + dy * dy);
            double th = atan2(dy, dx) + 0.012 * r - a;
            int u = (int)(r * cos(th));
            int v = (int)(r * sin(th));
            row[x] = pal[((u ^ v) + (int)(tsec * 30)) & 255];
        }
    }
}

static int julia_it(double zr, double zi, double cr, double ci)
{
    int n;
    for (n = 0; n < 40; n++) {
        double zr2 = zr * zr, zi2 = zi * zi;
        if (zr2 + zi2 > 4)
            return n;
        zi = 2 * zr * zi + ci;
        zr = zr2 - zi2 + cr;
    }
    return 40;
}

static void d_julia(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    double cr = -0.8 + 0.28 * sin(tsec * 0.23);
    double ci = 0.156 + 0.22 * cos(tsec * 0.19);
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        double zi0 = (y / (double)h) * 2.4 - 1.2;
        for (x = 0; x < w; x++) {
            double zr0 = (x / (double)w) * 3.2 - 1.6;
            int n = julia_it(zr0, zi0, cr, ci);
            row[x] = n >= 40 ? 0 : pal[n * 6];
        }
    }
}

static void d_lorenz(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    double x = 0.1, y = 0, z = 0;
    if (lor_n == 0) {
        x = 0.01;
        y = 0;
        z = 0;
        for (i = 0; i < 800; i++) {
            double dt = 0.008;
            double nx = x + dt * 10 * (y - x);
            double ny = y + dt * (x * (28 - z) - y);
            double nz = z + dt * (x * y - 8.0 / 3.0 * z);
            x = nx;
            y = ny;
            z = nz;
            lor[i][0] = (float)x;
            lor[i][1] = (float)y;
            lor[i][2] = (float)z;
        }
        lor_n = 800;
    }
    /* step the attractor */
    x = lor[(lor_i + 799) % 800][0];
    y = lor[(lor_i + 799) % 800][1];
    z = lor[(lor_i + 799) % 800][2];
    {
        double dt = 0.012;
        double nx = x + dt * 10 * (y - x);
        double ny = y + dt * (x * (28 - z) - y);
        double nz = z + dt * (x * y - 8.0 / 3.0 * z);
        lor[lor_i][0] = (float)nx;
        lor[lor_i][1] = (float)ny;
        lor[lor_i][2] = (float)nz;
        lor_i = (lor_i + 1) % 800;
    }
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 800; i++) {
        int sx = (int)(w / 2 + lor[i][0] * 7.5);
        int sy = (int)(h / 2 + 20 - lor[i][2] * 4.0);
        if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
            pix[sy * st + sx] = pal[(i + tick) & 255];
    }
}

static void lsys_draw(const char *s, double ang)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int w = (int)FB_W, h = (int)FB_H;
    double x = w * 0.5, y = h - 18, a = -1.5708;
    double stk[32][3];
    int sp = 0, i;
    double step = 6.0 / (lsys_gen);
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; s[i]; i++) {
        if (s[i] == 'F') {
            double nx = x + cos(a) * step * 14;
            double ny = y + sin(a) * step * 14;
            int x0 = (int)x, y0 = (int)y, x1 = (int)nx, y1 = (int)ny;
            int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
            int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
            int err = dx + dy;
            while (1) {
                if ((unsigned)x0 < (unsigned)w && (unsigned)y0 < (unsigned)h)
                    pix[y0 * st + x0] = pal[80 + lsys_gen * 20];
                if (x0 == x1 && y0 == y1)
                    break;
                {
                    int e2 = 2 * err;
                    if (e2 >= dy) {
                        err += dy;
                        x0 += sx;
                    }
                    if (e2 <= dx) {
                        err += dx;
                        y0 += sy;
                    }
                }
            }
            x = nx;
            y = ny;
        } else if (s[i] == '+')
            a += ang;
        else if (s[i] == '-')
            a -= ang;
        else if (s[i] == '[' && sp < 31) {
            stk[sp][0] = x;
            stk[sp][1] = y;
            stk[sp][2] = a;
            sp++;
        } else if (s[i] == ']' && sp) {
            sp--;
            x = stk[sp][0];
            y = stk[sp][1];
            a = stk[sp][2];
        }
    }
}

static void d_lsys(void)
{
    static char buf[8000], nxt[8000];
    int g, i;
    snprintf(buf, sizeof buf, "F");
    for (g = 0; g < lsys_gen && g < 6; g++) {
        int o = 0;
        nxt[0] = 0;
        for (i = 0; buf[i] && o < 7800; i++) {
            if (buf[i] == 'F') {
                memcpy(nxt + o, "F[+F]F[-F]F", 11);
                o += 11;
            } else
                nxt[o++] = buf[i];
        }
        nxt[o] = 0;
        memcpy(buf, nxt, (size_t)o + 1);
    }
    lsys_draw(buf, 0.44 + 0.08 * sin(tsec));
}

static void d_ifs(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    double x = 0, y = 0;
    if ((tick & 7) == 0)
        memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 1200; i++) {
        int r = rand() % 100;
        double nx, ny;
        if (r < 1) {
            nx = 0;
            ny = 0.16 * y;
        } else if (r < 86) {
            nx = 0.85 * x + 0.04 * y;
            ny = -0.04 * x + 0.85 * y + 1.6;
        } else if (r < 93) {
            nx = 0.2 * x - 0.26 * y;
            ny = 0.23 * x + 0.22 * y + 1.6;
        } else {
            nx = -0.15 * x + 0.28 * y;
            ny = 0.26 * x + 0.24 * y + 0.44;
        }
        x = nx;
        y = ny;
        {
            int sx = (int)(w / 2 + x * 28);
            int sy = (int)(h - 8 - y * 26);
            if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
                pix[sy * st + sx] = pal[100 + (i & 80)];
        }
    }
}

static void d_wave(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, x, y, w = (int)FB_W, h = (int)FB_H;
    if ((tick % 80) == 0)
        wave[rand() % 160] += 28;
    for (i = 1; i < 159; i++) {
        float f = wave[i - 1] + wave[i + 1] - 2 * wave[i];
        wave_v[i] += f * 0.15f;
        wave_v[i] *= 0.98f;
    }
    for (i = 1; i < 159; i++)
        wave[i] += wave_v[i];
    memset(pix, 0, (size_t)st * h * 2);
    for (x = 0; x < w; x++) {
        int wi = x * 159 / (w - 1);
        int mid = h / 2 + (int)wave[wi];
        for (y = 1; y < h; y++) {
            int d = y - mid;
            if (d < 0)
                d = -d;
            if (d < 3)
                pix[y * st + x] = pal[180 - d * 40];
            else if (y > mid)
                pix[y * st + x] = pal[20 + (y & 15)];
        }
    }
}

static void d_scroll(void)
{
    static const char *msg = "  *** SHARKDECK  cArt  plasma tunnel fire julia life ***  ";
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    int off = (int)(tsec * 40) % ((int)strlen(msg) * 6);
    for (y = 1; y < h; y++) {
        int bar = (sint[(y + (int)(tsec * 30)) & 255] + 128) >> 1;
        uint16_t c = pal[bar];
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++)
            row[x] = c;
    }
    {
        int n = (int)strlen(msg);
        char ch[2] = {0};
        int i;
        for (i = 0; i < n; i++) {
            int px0 = i * 12 - off;
            int yy = h / 2 + sint[(i * 8 + (int)(tsec * 50)) & 255] / 6;
            ch[0] = msg[i];
            if (px0 > -12 && px0 < w)
                text(px0, yy, ch, rgb565(255, 255, 200));
        }
    }
    (void)pix;
    (void)st;
}

static void d_glitch(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int y, w = (int)FB_W, h = (int)FB_H;
    d_plasma();
    if ((tick & 3) == 0) {
        int a = 10 + rand() % (h - 30);
        int b = a + 2 + rand() % 12;
        int sh = (rand() % 40) - 20;
        for (y = a; y < b && y < h; y++) {
            uint16_t tmp[480];
            int n = w < 480 ? w : 480;
            memcpy(tmp, pix + y * st, (size_t)n * 2);
            if (sh > 0)
                memcpy(pix + y * st + sh, tmp, (size_t)(n - sh) * 2);
            else
                memcpy(pix + y * st, tmp - sh, (size_t)(n + sh) * 2);
        }
    }
}

static void d_life(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    int p = life_p, q = p ^ 1;
    if ((tick & 1) == 0) {
        for (y = 1; y < 79; y++)
            for (x = 1; x < 119; x++) {
                int n = life[p][y - 1][x - 1] + life[p][y - 1][x] + life[p][y - 1][x + 1]
                      + life[p][y][x - 1] + life[p][y][x + 1]
                      + life[p][y + 1][x - 1] + life[p][y + 1][x] + life[p][y + 1][x + 1];
                life[q][y][x] = (n == 3) || (life[p][y][x] && n == 2);
            }
        life_p = q;
        p = q;
    }
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        int gy = y * 80 / h;
        if (gy > 79)
            gy = 79;
        for (x = 0; x < w; x++) {
            int gx = x * 120 / w;
            if (gx > 119)
                gx = 119;
            row[x] = life[p][gy][gx] ? pal[140] : 0;
        }
    }
}

static void d_xor(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H, t = (int)(tsec * 20);
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++)
            row[x] = pal[((x + t) ^ (y + t / 2)) & 255];
    }
}

static void d_moire(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    float ax = w / 2.0f + cosf((float)tsec) * 40;
    float ay = h / 2.0f + sinf((float)tsec * 1.3f) * 30;
    float bx = w / 2.0f - cosf((float)tsec * 0.8f) * 50;
    float by = h / 2.0f + cosf((float)tsec) * 25;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            float d1 = hypotf(x - ax, y - ay);
            float d2 = hypotf(x - bx, y - by);
            row[x] = pal[((int)d1 ^ (int)d2) & 255];
        }
    }
}

static void d_rings(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    float cx0 = w / 2.0f, cy0 = h / 2.0f;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            float d = hypotf(x - cx0, y - cy0);
            row[x] = pal[((int)(d * 0.4f + tsec * 40)) & 255];
        }
    }
}

static void d_rose(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    double k = 3 + 2 * sin(tsec * 0.15);
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 1200; i++) {
        double th = i * 0.02 + tsec;
        double r = 90 * sin(k * th);
        int sx = (int)(w / 2 + r * cos(th));
        int sy = (int)(h / 2 + r * sin(th));
        if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
            pix[sy * st + sx] = pal[(i + tick) & 255];
    }
}

static void d_lissa(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 600; i++) {
        double u = i * 0.03 + tsec;
        int sx = (int)(w / 2 + 160 * sin(u * 3));
        int sy = (int)(h / 2 + 90 * sin(u * 4 + 0.4));
        if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
            pix[sy * st + sx] = pal[i & 255];
    }
}

static void d_sierp(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    static float sx = 240, sy = 300;
    float ax[3], ay[3];
    ax[0] = w / 2.0f;
    ay[0] = 12;
    ax[1] = 12;
    ay[1] = h - 8;
    ax[2] = w - 12;
    ay[2] = h - 8;
    if ((tick & 15) == 0)
        memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 800; i++) {
        int r = rand() % 3;
        sx = (sx + ax[r]) * 0.5f;
        sy = (sy + ay[r]) * 0.5f;
        if ((unsigned)(int)sx < (unsigned)w && (unsigned)(int)sy < (unsigned)h)
            pix[(int)sy * st + (int)sx] = pal[90];
    }
}

static void d_rule30(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    uint8_t nxt[320];
    int n = w < 320 ? w : 320;
    if ((tick & 1) == 0) {
        for (x = 1; x < n - 1; x++) {
            int L = rule30[x - 1], C = rule30[x], R = rule30[x + 1];
            int v = (L << 2) | (C << 1) | R;
            nxt[x] = (uint8_t)((30 >> v) & 1);
        }
        memcpy(rule30, nxt, (size_t)n);
        memmove(pix + st, pix, (size_t)st * (h - 2) * 2);
        for (x = 0; x < w; x++)
            pix[(h - 1) * st + x] = rule30[x % n] ? pal[200] : 0;
    }
}

static void d_ant(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, x, y, w = (int)FB_W, h = (int)FB_H;
    const int dx[4] = {1, 0, -1, 0};
    const int dy[4] = {0, 1, 0, -1};
    for (i = 0; i < 40; i++) {
        uint8_t c = ant_g[ant_y][ant_x];
        ant_d = (ant_d + (c ? 1 : 3)) & 3;
        ant_g[ant_y][ant_x] = !c;
        ant_x += dx[ant_d];
        ant_y += dy[ant_d];
        if (ant_x < 1)
            ant_x = 118;
        if (ant_x > 118)
            ant_x = 1;
        if (ant_y < 1)
            ant_y = 78;
        if (ant_y > 78)
            ant_y = 1;
    }
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        int gy = y * 80 / h;
        if (gy > 79)
            gy = 79;
        for (x = 0; x < w; x++) {
            int gx = x * 120 / w;
            if (gx > 119)
                gx = 119;
            row[x] = ant_g[gy][gx] ? pal[40] : 0;
        }
    }
}

static void d_rain(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 80; i++) {
        int y, x = (int)drop_x[i];
        drop_y[i] += 6 + (i & 3);
        if (drop_y[i] > h) {
            drop_y[i] = 0;
            drop_x[i] = rand() % w;
        }
        for (y = 0; y < 8; y++) {
            int yy = (int)drop_y[i] - y;
            if ((unsigned)x < (unsigned)w && (unsigned)yy < (unsigned)h)
                pix[yy * st + x] = pal[160 - y * 10];
        }
    }
}

static void d_snow(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 120; i++) {
        int x, y;
        snow_y[i] += 0.6f + (i % 3) * 0.2f;
        snow_x[i] += sint[(tick + i) & 255] / 80.0f;
        if (snow_y[i] > h) {
            snow_y[i] = 0;
            snow_x[i] = rand() % w;
        }
        x = (int)snow_x[i];
        y = (int)snow_y[i];
        if ((unsigned)x < (unsigned)w && (unsigned)y < (unsigned)h)
            pix[y * st + x] = rgb565(220, 220, 230);
    }
}

static void d_static(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    unsigned s = 1103515245u * (tick + 1u);
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            s = s * 1664525u + 1013904223u;
            row[x] = pal[(s >> 16) & 255];
        }
    }
}

static void d_voronoi(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, i, w = (int)FB_W, h = (int)FB_H;
    float px0[8], py0[8];
    for (i = 0; i < 8; i++) {
        px0[i] = w / 2.0f + cosf((float)(tsec * (0.4 + i * 0.07) + i)) * (w * 0.35f);
        py0[i] = h / 2.0f + sinf((float)(tsec * (0.5 + i * 0.05) + i * 2)) * (h * 0.35f);
    }
    for (y = 1; y < h; y += 2) {
        uint16_t *row = pix + y * st;
        uint16_t *row2 = y + 1 < h ? pix + (y + 1) * st : row;
        for (x = 0; x < w; x += 2) {
            int best = 0;
            float bd = 1e9f;
            uint16_t c;
            for (i = 0; i < 8; i++) {
                float dx = x - px0[i], dy = y - py0[i], d = dx * dx + dy * dy;
                if (d < bd) {
                    bd = d;
                    best = i;
                }
            }
            c = pal[best * 28 + 20];
            row[x] = c;
            if (x + 1 < w)
                row[x + 1] = c;
            row2[x] = c;
            if (x + 1 < w)
                row2[x + 1] = c;
        }
    }
}

static void d_flow(void)
{
    static float fx[200], fy[200];
    static int on;
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    if (!on) {
        for (i = 0; i < 200; i++) {
            fx[i] = rand() % w;
            fy[i] = rand() % h;
        }
        on = 1;
    }
    if ((tick & 3) == 0)
        memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 200; i++) {
        float a = (float)sin(fx[i] * 0.03 + tsec) + (float)cos(fy[i] * 0.02);
        fx[i] += cosf(a) * 2.2f;
        fy[i] += sinf(a) * 2.2f;
        if (fx[i] < 0 || fx[i] >= w || fy[i] < 0 || fy[i] >= h) {
            fx[i] = rand() % w;
            fy[i] = rand() % h;
        }
        pix[(int)fy[i] * st + (int)fx[i]] = pal[(i + tick) & 255];
    }
}

static void d_bounce(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 12; i++) {
        int x, y, dx, dy;
        vx[i] += vdx[i];
        vy[i] += vdy[i];
        if (vx[i] < 6 || vx[i] > w - 6)
            vdx[i] = -vdx[i];
        if (vy[i] < 12 || vy[i] > h - 6)
            vdy[i] = -vdy[i];
        x = (int)vx[i];
        y = (int)vy[i];
        for (dy = -3; dy <= 3; dy++)
            for (dx = -3; dx <= 3; dx++)
                if (dx * dx + dy * dy <= 9 &&
                    (unsigned)(x + dx) < (unsigned)w &&
                    (unsigned)(y + dy) < (unsigned)h)
                    pix[(y + dy) * st + x + dx] = pal[i * 20];
    }
}

static void d_flag(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            int yy = y + sint[(x + (int)(tsec * 40)) & 255] / 10;
            int band = ((yy + h) % 48) < 24;
            row[x] = band ? pal[30] : pal[200];
        }
    }
}

static void d_dottun(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 24; i++) {
        float z = fmodf((float)(tsec * 40 + i * 18), 400.0f) + 8;
        int r = (int)(1800.0f / z);
        int cx0 = w / 2, cy0 = h / 2, a;
        if (r < 2)
            continue;
        for (a = 0; a < 64; a++) {
            double th = a * 6.2832 / 64.0;
            int sx = cx0 + (int)(r * cos(th));
            int sy = cy0 + (int)(r * sin(th) * 0.7);
            if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
                pix[sy * st + sx] = pal[220 - i * 6];
        }
    }
}

static void d_bars(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int y, x, w = (int)FB_W, h = (int)FB_H;
    for (y = 1; y < h; y++) {
        int v = sint[(y * 3 + (int)(tsec * 50)) & 255] + 128;
        uint16_t c = pal[v];
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++)
            row[x] = c;
    }
}

static void d_kaleid(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        int ay = y < h / 2 ? y : h - 1 - y;
        for (x = 0; x < w; x++) {
            int ax = x < w / 2 ? x : w - 1 - x;
            int v = sint[(ax + (int)(tsec * 20)) & 255] + sint[(ay * 2) & 255];
            row[x] = pal[(v + 256) & 255];
        }
    }
}

static void d_hop(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int i, w = (int)FB_W, h = (int)FB_H;
    double a = 0.4 + 0.2 * sin(tsec * 0.05), b = -0.1;
    if ((tick & 31) == 0)
        memset(pix, 0, (size_t)st * h * 2);
    for (i = 0; i < 600; i++) {
        double nx = hopx * hopx - hopy * hopy + a;
        double ny = 2 * hopx * hopy + b;
        hopx = (float)nx;
        hopy = (float)ny;
        {
            int sx = (int)(w / 2 + hopx * 90);
            int sy = (int)(h / 2 + hopy * 70);
            if ((unsigned)sx < (unsigned)w && (unsigned)sy < (unsigned)h)
                pix[sy * st + sx] = pal[120];
        }
    }
}

static void d_noise(void)
{
    uint16_t *pix = fb_pixels();
    unsigned st = fb_stride();
    int x, y, w = (int)FB_W, h = (int)FB_H;
    for (y = 1; y < h; y++) {
        uint16_t *row = pix + y * st;
        for (x = 0; x < w; x++) {
            int n = sint[(x / 4 + (int)(tsec * 8)) & 255]
                  + sint[(y / 3 + (int)(tsec * 5)) & 255]
                  + sint[(x / 9 + y / 7) & 255];
            row[x] = pal[(n + 384) & 255];
        }
    }
}

static void (*fn[NDEMO])(void) = {
    d_plasma, d_tunnel, d_roto, d_fire, d_meta, d_star, d_twirl,
    d_julia, d_lorenz, d_lsys, d_ifs, d_wave,
    d_scroll, d_glitch, d_life,
    d_xor, d_moire, d_rings, d_rose, d_lissa, d_sierp, d_rule30,
    d_ant, d_rain, d_snow, d_static, d_voronoi, d_flow, d_bounce,
    d_flag, d_dottun, d_bars, d_kaleid, d_hop, d_noise
};

static void io_open(void)
{
    struct termios t;
    tcgetattr(0, &oldt);
    t = oldt;
    t.c_lflag &= ~(ICANON | ECHO);
    t.c_cc[VMIN] = 0;
    t.c_cc[VTIME] = 0;
    tcsetattr(0, TCSANOW, &t);
    rawon = 1;
}

static void io_close(void)
{
    if (rawon)
        tcsetattr(0, TCSANOW, &oldt);
}

static void handle(unsigned char *b, int n)
{
    if (n <= 0)
        return;
    if (b[0] == 'x' || b[0] == 'X' || b[0] == 24)
        want_quit = 1;
    else if (b[0] == ' ')
        pause_on = !pause_on;
    else if (b[0] == 'c' || b[0] == 'C')
        pal_set(pkind + 1);
    else if (b[0] == 'n' || b[0] == 'N' || b[0] == 13)
        mode = (mode + 1) % NDEMO;
    else if (b[0] == 'p' || b[0] == 'P')
        mode = (mode + NDEMO - 1) % NDEMO;
    else if (b[0] == '+' || b[0] == '=') {
        if (mode == D_LSYS && lsys_gen < 6)
            lsys_gen++;
    } else if (b[0] == '-') {
        if (mode == D_LSYS && lsys_gen > 1)
            lsys_gen--;
    } else if (b[0] >= '1' && b[0] <= '9')
        mode = (b[0] - '1') % NDEMO;
    else if (b[0] == 27 && n >= 3 && b[2] == 'C')
        mode = (mode + 1) % NDEMO;
    else if (b[0] == 27 && n >= 3 && b[2] == 'D')
        mode = (mode + NDEMO - 1) % NDEMO;
}

int main(void)
{
    srand(1);
    if (fb_open() < 0) {
        fprintf(stderr, "cArt needs /dev/fb0\n");
        return 1;
    }
    io_open();
    init_tables();
    t0 = now_s();
    while (!want_quit) {
        unsigned char b[8];
        int n;
        if (!pause_on) {
            tsec = now_s() - t0;
            tick++;
            fn[mode]();
            hud();
            fb_flip_fast();
        }
        n = (int)read(0, b, sizeof b);
        if (n > 0)
            handle(b, n);
        else
            usleep(8000);
    }
    free(tun_a);
    free(tun_d);
    io_close();
    fb_close();
    return 0;
}
