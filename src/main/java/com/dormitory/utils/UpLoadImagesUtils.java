package com.dormitory.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * 图片存储工具类
 *
 * @author XXX
 * @since 2024-05-07
 */
@Component
@Slf4j
public class UpLoadImagesUtils {
    /**
     *
     */
    @Value("${base.img}")
    private String baseUrl;

    public String uploadFile(MultipartFile file) {
        String originalFilename = file.getOriginalFilename();
        String suffix = Objects.requireNonNull(originalFilename).substring(originalFilename.lastIndexOf("."));
        String name = Objects.requireNonNull(originalFilename).substring(0, originalFilename.lastIndexOf("."));
        // 该方法返回的为当前项目的工作目录，即在哪个地方启动的java线程
        String dirPath = System.getProperty("user.dir") + baseUrl;
        String fileName = String.valueOf(System.currentTimeMillis());
        fileName = name + "-" +fileName + suffix;
        String path = "/images/" + fileName;
        try {
            FileCopyUtils.copy(file.getInputStream(), Files.newOutputStream(Paths.get(dirPath + fileName)));
            // 将相对路径返回给前端
            return path;
        } catch (Exception e) {
            log.info("文件上传发送异常，异常信息为：[{}]",e.toString());
        }
        return "";
    }


}
