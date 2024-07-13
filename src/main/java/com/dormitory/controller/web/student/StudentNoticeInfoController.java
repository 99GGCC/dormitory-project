package com.dormitory.controller.web.student;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.NoticeInfoQry;
import com.dormitory.controller.vo.NoticeInfoVO;
import com.dormitory.service.NoticeInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * <p>
 * 公告信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/notice/info")
@Api(value = "StudentNoticeInfoController", tags = {"公告信息接口"})
public class StudentNoticeInfoController {
    /**
     * 公告信息Service
     */
    private final NoticeInfoService noticeInfoService;

    @ApiOperation("公告信息分页查询")
    @GetMapping("/page")
    public R<IPage<NoticeInfoVO>> pageByQry(NoticeInfoQry qry) {
        return R.success(noticeInfoService.pageByQry(qry));
    }

    @ApiOperation("公告信息列表查询")
    @GetMapping("/list")
    public R<List<NoticeInfoVO>> listByQry(NoticeInfoQry qry) {
        return R.success(noticeInfoService.listByQry(qry));
    }

    @ApiOperation("公告信息详情")
    @GetMapping("/detail/{noticeId}")
    public R<NoticeInfoVO> detail(@PathVariable @NotNull(message = "公告ID") Long noticeId) {
        return R.success(noticeInfoService.detailById(noticeId));
    }
}

