#include <chrono>
#include <climits>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <math.h>
#include <string.h>
#include <cstdlib>
#include <time.h>
#include <float.h>
#include <cstdio>

using namespace std;

typedef unsigned char uchar;
typedef unsigned long ulong;

typedef struct
{
    double red, green, blue;
} RGB_Pixel;

typedef struct
{
    int width, height;
    int size;
    RGB_Pixel* data;
} RGB_Image;

typedef struct
{
    int size;
    RGB_Pixel center;
} RGB_Cluster;

/* Mersenne Twister related constants */
#define N 624
#define M 397
#define MAXBIT 30
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */
#define MAX_RGB_DIST 195075
#define NUM_RUNS 100

static ulong mt[N]; /* the array for the state vector  */
static int mti = N + 1; /* mti == N + 1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
void init_genrand(ulong s)
{
    mt[0] = s & 0xffffffffUL;
    for (mti = 1; mti < N; mti++)
    {
        mt[mti] = (1812433253UL * (mt[mti - 1] ^ (mt[mti - 1] >> 30)) + mti);
        mt[mti] &= 0xffffffffUL;
    }
}

ulong genrand_int32(void)
{
    ulong y;
    static ulong mag01[2] = { 0x0UL, MATRIX_A };

    if (mti >= N)
    {
        int kk;
        if (mti == N + 1)
            init_genrand(5489UL);
        for (kk = 0; kk < N - M; kk++)
        {
            y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
            mt[kk] = mt[kk + M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (; kk < N - 1; kk++)
        {
            y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
            mt[kk] = mt[kk + (M - N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
        mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ mag01[y & 0x1UL];
        mti = 0;
    }

    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

double genrand_real2(void)
{
    return genrand_int32() * (1.0 / 4294967296.0);
}

/* Function for generating a bounded random integer between 0 and RANGE */
uint32_t bounded_rand(const uint32_t range)
{
    uint32_t x = (uint32_t)genrand_int32();
    uint64_t m = ((uint64_t)x) * ((uint64_t)range);
    uint32_t l = (uint32_t)m;

    if (l < range)
    {
        #pragma warning(push)
        #pragma warning(disable:4146)  // disable unary minus on unsigned warning

        uint32_t t = -range;

        #pragma warning(pop)

        if (t >= range)
        {
            t -= range;
            if (t >= range)
            {
                t %= range;
            }
        }

        while (l < t)
        {
            x = (uint32_t)genrand_int32();
            m = ((uint64_t)x) * ((uint64_t)range);
            l = (uint32_t)m;
        }
    }

    return (uint32_t)(m >> 32);
}

/* ---------- safer read_PPM (reads exactly 3 bytes per pixel) ---------- */
RGB_Image* read_PPM(const char* filename)
{
    uchar byte;
    char buff[64];
    int c, max_rgb_val;
    int i = 0;
    
    RGB_Image* img;

    FILE* fp = nullptr;
    fopen_s(&fp, "filename", "mode");
    if (!fp)
    {
        fprintf(stderr, "Unable to open file '%s'!\n", filename);
        exit(EXIT_FAILURE);
    }

    /* read image format */
    if (!fgets(buff, sizeof(buff), fp))
    {
        perror(filename);
        fclose(fp);
        exit(EXIT_FAILURE);
    }

    /* check the image format to make sure that it is binary */
    if (buff[0] != 'P' || buff[1] != '6')
    {
        fprintf(stderr, "Invalid image format (must be 'P6')!\n");
        fclose(fp);
        exit(EXIT_FAILURE);
    }

    img = (RGB_Image*)malloc(sizeof(RGB_Image));
    if (!img)
    {
        fprintf(stderr, "Unable to allocate memory!\n");
        fclose(fp);
        exit(EXIT_FAILURE);
    }

    /* skip comments */
    c = getc(fp);
    while (c == '#')
    {
        while (getc(fp) != '\n');
        c = getc(fp);
    }

    ungetc(c, fp);

    /* read image dimensions */
    if (fscanf_s(fp, "%d %d", &img->width, &img->height) != 2)
    {
        fprintf(stderr, "Invalid image dimensions ('%s')!\n", filename);
        fclose(fp);
        free(img);
        exit(EXIT_FAILURE);
    }

    /* read maximum component value */
    if (fscanf_s(fp, "%d", &max_rgb_val) != 1)
    {
        fprintf(stderr, "Invalid maximum R, G, B value ('%s')!\n", filename);
        fclose(fp);
        free(img);
        exit(EXIT_FAILURE);
    }

    /* validate maximum component value */
    if (max_rgb_val != 255)
    {
        fprintf(stderr, "'%s' is not a 24-bit image!\n", filename);
        fclose(fp);
        free(img);
        exit(EXIT_FAILURE);
    }

    /* skip single newline after header */
    while (fgetc(fp) != '\n');

    /* allocate memory for pixel data */
    img->size = img->height * img->width;
    img->data = (RGB_Pixel*)malloc(img->size * sizeof(RGB_Pixel));

    if (!img->data) {
        fprintf(stderr, "Unable to allocate memory for pixel data!\n");
        free(img);
        fclose(fp);
        exit(EXIT_FAILURE);
    }

    /* Read pixels in explicit loop (3 bytes per pixel) */
    for (i = 0; i < img->size; ++i)
    {
        if (fread(&byte, 1, 1, fp) != 1) { fprintf(stderr, "Unexpected EOF (R)\n"); break; }
        img->data[i].red = (double)byte;
        if (fread(&byte, 1, 1, fp) != 1) { fprintf(stderr, "Unexpected EOF (G)\n"); break; }
        img->data[i].green = (double)byte;
        if (fread(&byte, 1, 1, fp) != 1) { fprintf(stderr, "Unexpected EOF (B)\n"); break; }
        img->data[i].blue = (double)byte;
    }

    fclose(fp);

    if (i != img->size) {
        /* If truncated, shrink size to actual pixels read */
        img->size = i;
    }

    return img;
}

/* ---------- safer write_PPM (clamp + round) ---------- */
void write_PPM(const RGB_Image* img, const char* filename)
{
    uchar byte;

    FILE* fp = nullptr;
    fopen_s(&fp, filename, "wb");
    if (!fp)
    {
        perror("fopen"); // <-- FIX: better error message
        fprintf(stderr, "Unable to open file '%s'!\n", filename);
        exit(EXIT_FAILURE);
    }

    fprintf(fp, "P6\n");
    fprintf(fp, "%d %d\n", img->width, img->height);
    fprintf(fp, "%d\n", 255);

    for (int i = 0; i < img->size; i++)
    {
        double r = img->data[i].red;
        double g = img->data[i].green;
        double b = img->data[i].blue;

        if (r < 0.0) r = 0.0; if (r > 255.0) r = 255.0;
        if (g < 0.0) g = 0.0; if (g > 255.0) g = 255.0;
        if (b < 0.0) b = 0.0; if (b > 255.0) b = 255.0;

        byte = (uchar)round(r); fwrite(&byte, sizeof(uchar), 1, fp);
        byte = (uchar)round(g); fwrite(&byte, sizeof(uchar), 1, fp);
        byte = (uchar)round(b); fwrite(&byte, sizeof(uchar), 1, fp);
    }

    fclose(fp);
}

/* Function to generate random cluster centers. */
RGB_Cluster* gen_rand_centers(const RGB_Image* img, const int k) {
    RGB_Pixel rand_pixel;
    RGB_Cluster* cluster;

    cluster = (RGB_Cluster*)malloc(k * sizeof(RGB_Cluster));

    if (!cluster) {
        fprintf(stderr, "Unable to allocate memory for clusters!\n");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < k; i++) {
        /* Make the initial guesses for the centers */
        uint32_t idx = bounded_rand((uint32_t)img->size);
        rand_pixel = img->data[idx];

        cluster[i].center.red = rand_pixel.red;
        cluster[i].center.green = rand_pixel.green;
        cluster[i].center.blue = rand_pixel.blue;

        /* initialize cluster sizes */
        cluster[i].size = 0;
    }

    return(cluster);
}

/* Color quantization using the batch k-means algorithm */
void batch_kmeans(const RGB_Image* img, const int num_colors,
    const int max_iters, RGB_Cluster* clusters)
{
    RGB_Cluster* temp_sum = (RGB_Cluster*)malloc(num_colors * sizeof(RGB_Cluster));
    if (!temp_sum) {
        fprintf(stderr, "Unable to allocate memory for temp clusters!\n");
        exit(EXIT_FAILURE);
    }

    int* member = (int*)malloc(img->size * sizeof(int));
    if (!member) {
        fprintf(stderr, "Unable to allocate memory for members!\n");
        free(temp_sum);
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < img->size; ++i) member[i] = -1;

    for (int iter = 0; iter < max_iters; iter++) {
        /* Reset cluster accumulators */
        for (int i = 0; i < num_colors; i++) {
            temp_sum[i].center.red = 0.0;
            temp_sum[i].center.green = 0.0;
            temp_sum[i].center.blue = 0.0;
            temp_sum[i].size = 0;
            clusters[i].size = 0;
        }

        int num_changes = 0;

        /* Assign each pixel to the closest cluster */
        for (int p = 0; p < img->size; p++) {
            RGB_Pixel pixel = img->data[p];
            double min_dist = DBL_MAX;
            int closest_cluster = 0;

            for (int j = 0; j < num_colors; j++) {
                RGB_Pixel center = clusters[j].center;
                double dr = pixel.red - center.red;
                double dg = pixel.green - center.green;
                double db = pixel.blue - center.blue;
                double dist = dr * dr + dg * dg + db * db;

                if (dist < min_dist) {
                    min_dist = dist;
                    closest_cluster = j;
                }
            }

            if (member[p] != closest_cluster) {
                num_changes++;
                member[p] = closest_cluster;
            }

            temp_sum[closest_cluster].size++;
            temp_sum[closest_cluster].center.red += pixel.red;
            temp_sum[closest_cluster].center.green += pixel.green;
            temp_sum[closest_cluster].center.blue += pixel.blue;
        }

        if (num_changes == 0) {
            break;  // convergence reached
        }

        /* Recompute centers using temp_sum counts (safe divide) */
        for (int i = 0; i < num_colors; i++) {
            if (temp_sum[i].size > 0) {
                clusters[i].center.red = temp_sum[i].center.red / (double)temp_sum[i].size;
                clusters[i].center.green = temp_sum[i].center.green / (double)temp_sum[i].size;
                clusters[i].center.blue = temp_sum[i].center.blue / (double)temp_sum[i].size;
                clusters[i].size = temp_sum[i].size;
            }
            // Clamp values to [0, 255]
            clusters[i].center.red = min(255.0, max(0.0, clusters[i].center.red));
            clusters[i].center.green = min(255.0, max(0.0, clusters[i].center.green));
            clusters[i].center.blue = min(255.0, max(0.0, clusters[i].center.blue));

            // <-- DEBUG print cluster centers
            printf("DEBUG: Cluster %d center -> R: %.2f, G: %.2f, B: %.2f (size: %d)\n",
                i, clusters[i].center.red, clusters[i].center.green,
                clusters[i].center.blue, clusters[i].size);
            fflush(stdout);
        }
    }

    free(temp_sum);
    free(member);
}

void free_img(RGB_Image* img) {
    if (!img) return;
    if (img->data) free(img->data);
    free(img);
}

RGB_Image* map_img(const RGB_Image* in_img, const RGB_Cluster* clusters, const int num_colors) {
    RGB_Image* out_img = (RGB_Image*)malloc(sizeof(RGB_Image));
    if (!out_img) {
        fprintf(stderr, "Unable to allocate memory for output image!\n");
        exit(EXIT_FAILURE);
    }

    out_img->width = in_img->width;
    out_img->height = in_img->height;
    out_img->size = in_img->size;
    out_img->data = (RGB_Pixel*)malloc(out_img->size * sizeof(RGB_Pixel));
    if (!out_img->data) {
        fprintf(stderr, "Unable to allocate memory for output image data!\n");
        free(out_img);
        exit(EXIT_FAILURE);
    }

    /* For each pixel in the input image */
    for (int i = 0; i < in_img->size; i++) {
        RGB_Pixel pixel = in_img->data[i];
        double min_dist = DBL_MAX;
        int closest_cluster = 0;

        /* Find the nearest cluster center */
        for (int j = 0; j < num_colors; j++) {
            RGB_Pixel center = clusters[j].center;
            double delta_red = pixel.red - center.red;
            double delta_green = pixel.green - center.green;
            double delta_blue = pixel.blue - center.blue;
            double dist = delta_red * delta_red + delta_green * delta_green + delta_blue * delta_blue;

            if (dist < min_dist) {
                min_dist = dist;
                closest_cluster = j;
            }
        }

        /* Set pixel in out_img to nearest cluster center color */
        out_img->data[i].red = clusters[closest_cluster].center.red;
        out_img->data[i].green = clusters[closest_cluster].center.green;
        out_img->data[i].blue = clusters[closest_cluster].center.blue;

        // <-- DEBUG: first few pixel mappings
        if (i < 10) {
            printf("DEBUG: Pixel %d -> Cluster %d, Color: R=%.2f, G=%.2f, B=%.2f\n",
                i, closest_cluster,
                out_img->data[i].red,
                out_img->data[i].green,
                out_img->data[i].blue);
            fflush(stdout);
        }
    }

    return out_img;
}

int main(int argc, char* argv[])
{
    char* filename;                     /* Filename Pointer*/
    int k;                              /* Number of clusters*/
    RGB_Image* img;
    RGB_Image* out_img;
    RGB_Cluster* cluster;
    std::chrono::high_resolution_clock::time_point start, stop;
    std::chrono::milliseconds elapsed;

    if (argc == 3) {
        filename = argv[1];
        k = atoi(argv[2]);
    }
    else if (argc > 3) {
        printf("Too many arguments supplied.\n");
        return 0;
    }
    else {
        printf("Two arguments expected: image filename and number of clusters.\n");
        return 0;
    }

    init_genrand((ulong)time(NULL));

    printf("%s %d\n", filename, k);
    fflush(stdout);

    img = read_PPM(filename);

    start = std::chrono::high_resolution_clock::now();

    cluster = gen_rand_centers(img, k);

    FILE* log = nullptr;
    fopen_s(&log, "cluster_centers_init.txt", "w");

    if (log) {
        for (int i = 0; i < k; i++) {
            fprintf(log, "Initial Cluster %d: R: %.2f, G: %.2f, B: %.2f\n", i + 1,
                cluster[i].center.red,
                cluster[i].center.green,
                cluster[i].center.blue);
        }
        fclose(log);
    }


    batch_kmeans(img, k, INT_MAX, cluster);

    for (int i = 0; i < k; i++) {
        printf("Cluster %d: R: %.2f, G: %.2f, B: %.2f\n", i + 1,
            cluster[i].center.red, cluster[i].center.green, cluster[i].center.blue);
        fflush(stdout);
    }

    stop = std::chrono::high_resolution_clock::now();

    FILE* fp = log;
    fopen_s(&log, "cluster_centers_final.txt", "w");
    if (log) {
        for (int i = 0; i < k; i++) {
            fprintf(log, "Final Cluster %d: R: %.2f, G: %.2f, B: %.2f\n", i + 1,
                cluster[i].center.red,
                cluster[i].center.green,
                cluster[i].center.blue);
        }
        fclose(log);
    }

    out_img = map_img(img, cluster, k);

    // Change path if needed:
    write_PPM(out_img, "output.ppm");

    elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    free_img(out_img);
    free(cluster);
    free_img(img);

    return 0;
}
