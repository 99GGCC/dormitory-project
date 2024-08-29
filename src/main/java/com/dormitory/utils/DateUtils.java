package com.dormitory.utils;

import com.dormitory.common.Constant;
import lombok.SneakyThrows;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 日期工具类
 *
 * @author XXX
 */
public class DateUtils {

    private DateUtils() {
        throw new IllegalStateException("DateUtils class");
    }

    /**
     * 日期转字符串
     *
     * @param date 传入日期
     * @return String类型日期
     */
    public static String dateToString1(Date date) {
        //格式化日期时间
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        return sdf.format(date);
    }


    /**
     * 日期转字符串
     *
     * @param date 传入日期
     * @return String类型日期
     */
    public static String dateToString(Date date) {
        //格式化日期时间
        SimpleDateFormat sdf = new SimpleDateFormat(Constant.DATE_YYYY_MM_DD_HH_MM_SS);
        return sdf.format(date);
    }


    /**
     * 日期转字符串
     *
     * @param date 传入日期
     * @return String类型日期
     */
    public static String dateToString2(Date date) {
        //格式化日期时间
        SimpleDateFormat sdf = new SimpleDateFormat(Constant.DATE_YYYY_MM_DD);
        return sdf.format(date);
    }

    /**
     * 设置时间格式
     *
     * @param date 传入时间
     * @param type 0、设置为00:00:00:000 1、设置为23:59:59:999
     * @return Date
     */
    public static Date setDate(Date date, Integer type) {
        if (ObjectUtils.isEmpty(date)) {
            return null;
        }
        if (Constant.INTEGER_ZERO.equals(type)) {
            // 设置开始时间为当天的0点0分0秒
            Calendar startCal = Calendar.getInstance();
            startCal.setTime(date);
            startCal.set(Calendar.HOUR_OF_DAY, 0);
            startCal.set(Calendar.MINUTE, 0);
            startCal.set(Calendar.SECOND, 0);
            startCal.set(Calendar.MILLISECOND, 0);
            date = startCal.getTime();
        }
        if (Constant.INTEGER_ONE.equals(type)) {
            // 设置结束时间为当天的23时59分59秒
            Calendar endCal = Calendar.getInstance();
            endCal.setTime(date);
            endCal.set(Calendar.HOUR_OF_DAY, 23);
            endCal.set(Calendar.MINUTE, 59);
            endCal.set(Calendar.SECOND, 59);
            endCal.set(Calendar.MILLISECOND, 999);
            date = endCal.getTime();
        }
        return date;
    }

    /**
     * 计算当前时间到给定时间之间的秒数。
     *
     * @param futureDate 未来的时间点
     * @return 当前时间到给定时间之间的秒数
     */
    public static long calculateSecondsUntil(Date futureDate) {
        // 获取当前时间的 Instant 对象
        Instant now = Instant.now();
        // 将 Date 对象转换为 Instant 对象
        Instant future = futureDate.toInstant();
        // 计算两个 Instant 对象之间的持续时间
        Duration duration = Duration.between(now, future);
        // 返回持续时间的秒数
        return duration.getSeconds();
    }
}
