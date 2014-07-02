package in.dogue.gazophylacium.audio.synth;

/*
 * This is a simple speech synth subroutine, based on formant synthesis theory.
 * Speech is synthesized by passing source excitation signal through set of formant one-pole filters.
 * Excitation signal is a sawtooth or noise (depending on sound type), although you can try other signals.
 * Created by:           Stepanov Andrey, 2008 ( ICQ: 129179794, e-mail: andrewstepanov@mail.ru )
 */
public class TinySynth {
    static final int SAMPLE_FREQUENCY = 44100;
    static final int PLAY_TIME = 32;
    static final double MASTER_VOLUME = 0.0003;
    static final double PI = 3.141592653589793;
    static final double PI_2 = 2*PI;

    private static double sawtooth(double x) {
        return 0.5 - (x - Math.floor(x / PI_2) * PI_2) / PI_2;
    }

    public static short[] generate(String s) {
        double[] buf = new double[PLAY_TIME*SAMPLE_FREQUENCY*2];

        int k = synthSpeech(buf, 0, s);
        short[] finalBuf = new short[k/2];

        for (int i = 0; i < k/2; i += 1) {
            double f0 = buf[i*2];
            double f1 = buf[i*2 + 1];
            finalBuf[i] = (short)(32700.0 * Helpers.cutLevel((f0 + f1) / 2 * MASTER_VOLUME, 1));
        }
        return finalBuf;
    }

    private static int synthSpeech(double[] buf, int k, String text) {
        for (int i = 0; i < text.length(); i++){
            char l = text.charAt(i);
            double v = 0;
            Phoneme p = Phoneme.find(l);
            if (l != ' ') {
                v = p.shape().amp();
            }

            int sl = p.shape().len() * (SAMPLE_FREQUENCY / 15);
            for (int f = 0;  f < 3; f += 1) {
                int ff = p.f().apply(f);
                double freq = (double)ff * (50.0/SAMPLE_FREQUENCY);
                if (ff == 0) {
                    continue;
                }
                double buf1Res = 0;
                double buf2Res = 0;
                double q = 1.0 - p.w().apply(f) * (PI * 10.0 / SAMPLE_FREQUENCY);
                int j = 0;
                double xp = 0;
                for (int s = 0; s < sl; s += 1) {
                    double n = Math.random() - 0.5;
                    double x = n;
                    if (!p.shape().osc()) {
                        x = sawtooth(s * (120.0 * PI_2 / SAMPLE_FREQUENCY));
                        xp = 0;
                    }
                    x = x + 2.0 * Math.cos(PI_2 * freq) * buf1Res * q - buf2Res * q * q;
                    buf2Res = buf1Res;
                    buf1Res = x;
                    x = 0.75 * xp + x * v;
                    xp = x;
                    x *= Helpers.cutLevel(Math.sin((PI * s) / sl) * 5, 1);
                    buf[k+j] += x;
                    j += 1;
                    buf[k+j] += x;
                    j += 1;
                }
            }

            k += ((3*sl/4)<<1);
            if (p.shape().plosive()) {
                k += (sl & 0xfffffe);
            }

        }
        return k;
    }
}



