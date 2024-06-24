package com.dormitory.controller;

import com.alibaba.fastjson.JSON;
import com.dormitory.common.Constant;
import com.dormitory.common.R;
import com.dormitory.exception.ServiceException;
import com.dormitory.service.NoticeInfoService;
import com.dormitory.utils.IdUtils;
import com.dormitory.utils.ImageCodeUtils;
import com.dormitory.utils.RedisUtil;
import com.dormitory.utils.UpLoadImagesUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import javax.validation.constraints.NotNull;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * <p>
 * 共通接口 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2022-05-07
 */
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/common")
@Api(value = "CommonController", tags = {"共通方法接口"})
public class CommonController {

    /**
     * redis工具类
     */
    private final RedisUtil redisUtil;

    /**
     * 图片存储工具类
     */
    private final UpLoadImagesUtils upLoadImagesUtils;

    /**
     * 文件存储位置
     */
    @Value("${base.img}")
    private String baseImg;

    /**
     * 公告Service
     */
    private final NoticeInfoService noticeInfoService;

    @ApiOperation("公告分页查询")
    @GetMapping("/page")
    public R<Boolean> page() {
        Map<String, Object> map = new HashMap<>();
        map.put("notice_title", "测试");
        noticeInfoService.removeByMap(map);
        return R.success();
    }

    @GetMapping(value = "/getImageCode")
    @ApiOperation(value = "获取图片验证码", notes = "获取图片验证码")
    public R<String> getImageCode() {
        Map<String, String> map = ImageCodeUtils.imageCode(120, 40);
        // 获取验证码值，存入redis
        String code = map.get(Constant.CODE).toLowerCase();
        redisUtil.set(Constant.SMS_CODE + code, code, Constant.FIVE_MINUTE);
        // 返回验证码图片
        return R.success(map.get(Constant.CODE_VALUE));
    }

    @GetMapping(value = "/getKeyId/{number}")
    @ApiOperation(value = "获取keyId", notes = "获取keyId")
    public R<List<Long>> getKeyId(@PathVariable @NotNull(message = "数量不能为空") Integer number) {
        List<Long> keyIds = new ArrayList<>();
        for (int i = 0; i < number; i++) {
            keyIds.add(IdUtils.getLongId());
        }
        return R.success(keyIds);
    }

    @SneakyThrows
    @PostMapping(value = "/upload/image")
    @ApiOperation(value = "图片文件上传", notes = "图片文件上传")
    public R<String> uploadImageFile(@ApiParam(value = "文件上传文件名:file", required = true) @RequestPart @RequestParam("file") MultipartFile file) {
        String path;
        if (ImageIO.read(file.getInputStream()) != null) {
            //传入转码文件
            path = upLoadImagesUtils.uploadFile(file);
            if (StringUtils.isNotBlank(path)) {
                return R.success(Constant.SUCCESS_CODE, "图片上传成功!", path);
            } else {
                return R.fail(Constant.FAIL_CODE, "图片上传失败!");
            }
        } else {
            return R.fail(Constant.FAIL_CODE, "请上传图片！");
        }
    }

    @SneakyThrows
    @PostMapping(value = "/images/delete")
    @ApiOperation(value = "图片删除", notes = "图片删除")
    public R<Boolean> imagesDelete(@ApiParam(value = "文件地址", required = true) @RequestParam("url") String url) {
        try {
            String property = System.getProperty("user.dir");
            String fileUrl = (property + baseImg + url).replace("//images/", "/");
            boolean status = Files.deleteIfExists(Path.of(fileUrl));
            return R.success(status);
        } catch (Exception e) {
            return R.fail(false);
        }
    }

    @SneakyThrows
    @PostMapping(value = "/upload/batch/image")
    @ApiOperation(value = "图片文件批量上传", notes = "图片文件批量上传")
    public R<List<String>> uploadImageFiles(@ApiParam(value = "文件上传文件名:files", required = true) @RequestPart @RequestParam("files") List<MultipartFile> files) {
        List<String> path = new ArrayList<>();
        if (CollectionUtils.isEmpty(files)) {
            throw new ServiceException("上传文件为空!");
        }
        try {
            for (MultipartFile file : files) {
                if (ImageIO.read(file.getInputStream()) != null) {
                    //传入转码文件
                    path.add(upLoadImagesUtils.uploadFile(file));
                } else {
                    log.info("文件解析失败，请检查文件后缀名，文件为[{}]", JSON.toJSONString(file));
                    throw new ServiceException("文件解析异常，请检查文件名或文件后再试!");
                }
            }
            if (CollectionUtils.isNotEmpty(path)) {
                return R.success(Constant.SUCCESS_CODE, "图片批量上传成功!", path);
            } else {
                return R.fail(Constant.FAIL_CODE, "图片上传失败!");
            }
        } catch (Exception e) {
            if (CollectionUtils.isNotEmpty(path)) {
                String property = System.getProperty("user.dir");
                for (String filePath : path) {
                    String fileUrl = (property + baseImg + filePath).replace("//images/", "/");
                    Files.deleteIfExists(Path.of(fileUrl));
                }
            }
            throw new ServiceException("文件上传发生异常，请检查文件名或文件后再试!");
        }
    }
}

