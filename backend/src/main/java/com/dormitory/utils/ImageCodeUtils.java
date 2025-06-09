package com.dormitory.utils;

import cn.hutool.core.codec.Base64;
import lombok.SneakyThrows;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * 验证码生成工具类
 *
 * @author XXX
 */
public class ImageCodeUtils {

    /**
     * 随机数
     */
    private static final Random RAND = new Random();

    private ImageCodeUtils() {
    }

    /**
     * 验证码长度
     */
    private static final int CODE_LEN = 4;

    /**
     * 生成一个验证码图片，并返回一个包含验证码字符和验证码图片（以 Base64 编码形式）的 Map
     */
    @SneakyThrows
    public static Map<String, String> imageCode(Integer width, Integer height) {
        // 用于绘制图片，设置图片的长宽和图片类型（RGB)
        BufferedImage code = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        // 获取绘图工具
        Graphics graphics = code.getGraphics();
        // 使用RGB设置背景颜色
        graphics.setColor(new Color(getNumber(255), getNumber(255), getNumber(255)));
        // 填充矩形区域
        graphics.fillRect(0, 0, width, height);
        // 验证码中所使用到的字符
        char[] codeChar = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456".toCharArray();
        // 存放生成的验证码
        StringBuilder captcha = new StringBuilder();
        // 循环将每个验证码字符绘制到图片上
        for (int i = 0; i < CODE_LEN; i++) {
            int index = getNumber(codeChar.length);
            Font drawFont = new Font("Arial", Font.BOLD, 20);
            // 随机生成验证码颜色
            graphics.setColor(new Color(getNumber(255), getNumber(255), getNumber(255)));
            graphics.setFont(drawFont);
            // 将一个字符绘制到图片上，并制定位置（设置x,y坐标）
            graphics.drawString(String.valueOf(codeChar[index]), (i * 20) + 15, 30);
            captcha.append(codeChar[index]);
        }
        //将图片转换陈字符串给前端、通过ImageIO将图片输出
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        ImageIO.write(code, "png", stream);
        String base64 = Base64.encode(stream.toByteArray());
        stream.flush();
        stream.close();
        Map<String, String> map = new HashMap<>(2);
        map.put("code", captcha.toString());
        map.put("codeValue", "data:image/png;base64," + base64);
        return map;
    }

    @SneakyThrows
    private static int getNumber(int num) {
        // 范围是[0,255)
        return RAND.nextInt(num);
    }
}
