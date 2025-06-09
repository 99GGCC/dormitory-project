package com.dormitory.utils;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * ID生成工具类
 *
 * @author XXX
 * @since 2024-05-07
 */
public class IdUtils {

    private IdUtils() {
        throw new IllegalStateException("IdUtils class");
    }

    /**
     * 生成主键(17位数字)
     * 主键生成方式,年月日时分秒毫秒的时间戳 例如：180910094204299
     */
    public static synchronized Long getLongId() {
        try {
            Thread.sleep(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        String msg = "";
        Date date = new Date();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmssSSS");
        msg = sdf.format(date).substring(2);
        return Long.parseLong(msg);
    }
}
