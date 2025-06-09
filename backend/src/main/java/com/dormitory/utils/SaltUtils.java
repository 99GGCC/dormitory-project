package com.dormitory.utils;

import java.security.MessageDigest;
import java.util.Base64;
import java.util.UUID;

/**
 * 盐加密工具类
 *
 * @author XXX
 * @since 2024-05-07
 */
public class SaltUtils {
    private SaltUtils() {
        throw new IllegalStateException("SaltUtils class");
    }

    /**
     * 获取盐，随机生成
     *
     * @return
     */
    public static String createSalt() {
        // 生成全球唯一的id
        UUID randomUuId = UUID.randomUUID();
        return randomUuId.toString();
    }

    /**
     * 密码加密
     *
     * @param password
     * @param salt
     * @return
     */
    public static String md5Password(String password, String salt) {
        try {
            //获取MD5加密算法
            MessageDigest digest = MessageDigest.getInstance("md5");
            //进行密码加密
            byte[] md5Password = digest.digest((password + salt).getBytes());
            Base64.Encoder encoder = Base64.getEncoder();
            return encoder.encodeToString(md5Password);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("加密失败!");
        }
    }

    /**
     * 密码对比
     *
     * @param password    未加密密码
     * @param salt        盐加密
     * @param passwordMd5 加密后密码
     * @return 密码是否相同
     */
    public static boolean verify(String password, String salt, String passwordMd5) {
        return md5Password(password, salt).equals(passwordMd5);
    }
}
