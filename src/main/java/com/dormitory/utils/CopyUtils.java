package com.dormitory.utils;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * 复制工具类
 *
 * @author XXX
 * @since 2024-05-07
 */
public class CopyUtils {

    private CopyUtils() {
        throw new IllegalStateException("CopyUtils class");
    }

    /**
     * 对象复制
     *
     * @param from  来源
     * @param clazz 输出Class
     * @param <T>   输出泛型
     * @return t 输出对象
     */
    @SneakyThrows
    public static <T> T classCopy(Object from, Class<T> clazz) {
        T t = clazz.getConstructor().newInstance();
        BeanUtils.copyProperties(from, t);
        return t;
    }

    /**
     * 对象复制
     *
     * @param from 来源
     * @param to   输出
     * @param <T>  输出泛型
     */
    @SneakyThrows
    public static <T> void classCopy(Object from, T to) {
        BeanUtils.copyProperties(from, to);
    }

    /**
     * List对象复制
     *
     * @param from   来源
     * @param tClass 输出类型
     * @param <T>    来源泛型
     * @param <E>    输出泛型
     */
    @SneakyThrows
    public static <T, E> List<E> classCopyList(List<T> from, Class<E> tClass) {
        List<E> to = new ArrayList<>();
        for (T t : from) {
            E e = tClass.getConstructor().newInstance();
            BeanUtils.copyProperties(t, e);
            to.add(e);
        }
        return to;
    }

    /**
     * 分页转换
     *
     * @param in     输入分页
     * @param eClass 输出分页class
     * @return IPage<E>
     */
    public static <T, E> IPage<E> covertPage(IPage<T> in, Class<E> eClass) {
        IPage<E> out = new Page<>(in.getCurrent(), in.getSize());
        out.setTotal(in.getTotal());
        out.setPages(in.getPages());
        out.setRecords(classCopyList(in.getRecords(), eClass));
        return out;
    }
}
